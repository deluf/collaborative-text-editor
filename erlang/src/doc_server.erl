-module(doc_server).
-behaviour(gen_server).

%% API
-export([start_link/1, join/2, get_text/1, request_sync/2]).
-export([add_char/5, remove_char/4, move_cursor/4]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SAVE_EVERY, 50).
-define(SAVE_INTERVAL, 30000).
-define(MAX_ACTIVE, 2).

-record(state, {
    doc_id, 
    doc = crdt_core:new(),
    cursors = #{},
    active = [],
    queue = [],
    pid_users = #{},
    op_count = 0
}).
-record(editor_docs, {doc_id, content}).

%%%===================================================================
%%% Client API
%%%===================================================================

start_link(DocId) ->
    gen_server:start_link({global, {doc, DocId}}, ?MODULE, [DocId], []).

join(DocId, Pid) ->
    gen_server:cast({global, {doc, DocId}}, {join, Pid}).

request_sync(DocId, Pid) ->
    gen_server:cast({global, {doc, DocId}}, {sync_req, Pid}).

get_text(DocId) ->
    gen_server:call({global, {doc, DocId}}, get_text).

add_char(DocId, SenderPid, Id, User, Char) ->
    gen_server:cast({global, {doc, DocId}}, {insert, SenderPid, Id, User, Char}).

remove_char(DocId, SenderPid, Id, User) ->
    gen_server:cast({global, {doc, DocId}}, {delete, SenderPid, Id, User}).

move_cursor(DocId, SenderPid, User, Pos) ->
    gen_server:cast({global, {doc, DocId}}, {move, SenderPid, User, Pos}).

%%%===================================================================
%%% GenServer Callbacks
%%%===================================================================

init([DocId]) ->
    InitialDoc = case mnesia:dirty_read(editor_docs, DocId) of
        [] -> crdt_core:new();
        [#editor_docs{content = SavedDoc}] -> SavedDoc
    end,
    erlang:send_after(?SAVE_INTERVAL, self(), trigger_save),
    {ok, #state{doc_id = DocId, doc = InitialDoc, cursors = #{}, pid_users = #{}, op_count = 0, active = [], queue = []}}.

handle_call(get_text, _From, State) ->
    Text = crdt_core:to_string(State#state.doc),
    {reply, Text, State}.

handle_cast({join, Pid}, State) ->
    erlang:monitor(process, Pid),
    ActiveCount = length(State#state.active),
    
    if 
        ActiveCount < ?MAX_ACTIVE ->
            CursorList = maps:to_list(State#state.cursors),
            Pid ! {sync_state, State#state.doc, CursorList},
            {noreply, State#state{active = [Pid | State#state.active]}};
        
        true ->
            QueuePos = length(State#state.queue), %% 0 based (0 = primo in attesa)
            Pid ! {queue_update, QueuePos},
            {noreply, State#state{queue = State#state.queue ++ [Pid]}}
    end;

handle_cast({sync_req, Pid}, State) ->
    case lists:member(Pid, State#state.active) of
        true ->
            CursorList = maps:to_list(State#state.cursors),
            Pid ! {sync_state, State#state.doc, CursorList};
        false ->
            ok
    end,
    {noreply, State};

handle_cast({insert, SenderPid, Id, UserId, Char}, State) ->
    case lists:member(SenderPid, State#state.active) of
        true ->
            NewPidUsers = maps:put(SenderPid, UserId, State#state.pid_users),
            NewDoc = crdt_core:insert(State#state.doc, Id, Char),
            broadcast(State#state.active, SenderPid, {insert, Id, UserId, Char}),
            {noreply, maybe_save(State#state{doc = NewDoc, pid_users = NewPidUsers})};
        false ->
            {noreply, State}
    end;

handle_cast({delete, SenderPid, Id, UserId}, State) ->
    case lists:member(SenderPid, State#state.active) of
        true ->
            NewPidUsers = maps:put(SenderPid, UserId, State#state.pid_users),
            NewDoc = crdt_core:delete(State#state.doc, Id),
            broadcast(State#state.active, SenderPid, {delete, Id, UserId}),
            {noreply, maybe_save(State#state{doc = NewDoc, pid_users = NewPidUsers})};
        false ->
            {noreply, State}
    end;

handle_cast({move, SenderPid, UserId, Pos}, State) ->
    case lists:member(SenderPid, State#state.active) of
        true ->
            NewPidUsers = maps:put(SenderPid, UserId, State#state.pid_users),
            NewCursors = maps:put(UserId, Pos, State#state.cursors),
            broadcast(State#state.active, SenderPid, {move, UserId, Pos}),
            {noreply, State#state{cursors = NewCursors, pid_users = NewPidUsers}};
        false ->
            {noreply, State}
    end.

handle_info(trigger_save, State) ->
    erlang:send_after(?SAVE_INTERVAL, self(), trigger_save),
    NewState = case State#state.op_count > 0 of
        true ->
            mnesia:dirty_write(#editor_docs{
                doc_id = State#state.doc_id, 
                content = State#state.doc
            }),
            State#state{op_count = 0};
        false ->
            State
    end,
    {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    IsActive = lists:member(Pid, State#state.active),
    
    if 
        IsActive ->
            NewActivePre = lists:delete(Pid, State#state.active),

            {UserToRemove, NewPidUsers} = case maps:find(Pid, State#state.pid_users) of
                {ok, User} -> {User, maps:remove(Pid, State#state.pid_users)};
                error -> {undefined, State#state.pid_users}
            end,
            NewCursors = case UserToRemove of
                undefined -> State#state.cursors;
                _ ->
                    broadcast(NewActivePre, Pid, {remove_cursor, UserToRemove}),
                    maps:remove(UserToRemove, State#state.cursors)
            end,

            {NewActive, NewQueue} = case State#state.queue of
                [] -> 
                    {NewActivePre, []};
                [PromotedPid | RestQueue] ->
                    CursorList = maps:to_list(NewCursors),
                    PromotedPid ! {sync_state, State#state.doc, CursorList},
                    notify_queue_positions(RestQueue),
                    {[PromotedPid | NewActivePre], RestQueue}
            end,

            case NewActive of
                [] ->
                    io:format("All disconnected~n"),
                    mnesia:dirty_write(#editor_docs{doc_id = State#state.doc_id, content = State#state.doc}),
                    {stop, normal, State#state{active = [], queue = [], cursors = NewCursors, pid_users = NewPidUsers}};
                _ ->
                    {noreply, State#state{active = NewActive, queue = NewQueue, cursors = NewCursors, pid_users = NewPidUsers}}
            end;

        true ->
            IsQueued = lists:member(Pid, State#state.queue),
            if 
                IsQueued ->
                    NewQueue = lists:delete(Pid, State#state.queue),
                    notify_queue_positions(NewQueue),
                    {noreply, State#state{queue = NewQueue}};
                true ->
                    {noreply, State}
            end
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

broadcast(ActiveClients, SenderPid, Msg) ->
    lists:foreach(fun(Pid) -> 
        if Pid =/= SenderPid -> Pid ! Msg; true -> ok end 
    end, ActiveClients).

notify_queue_positions(Queue) ->
    notify_queue_recursive(Queue, 0).

notify_queue_recursive([], _) -> ok;
notify_queue_recursive([Pid | Rest], Pos) ->
    Pid ! {queue_update, Pos},
    notify_queue_recursive(Rest, Pos + 1).

maybe_save(State) ->
    Current = State#state.op_count + 1,
    if 
        Current >= ?SAVE_EVERY ->
            mnesia:dirty_write(#editor_docs{
                doc_id = State#state.doc_id, 
                content = State#state.doc
            }),
            State#state{op_count = 0};
        true ->
            State#state{op_count = Current}
    end.