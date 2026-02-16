-module(doc_server).
-behaviour(gen_server).

%% API
-export([start_link/1, join/2, get_text/1]).
-export([add_char/5, remove_char/4, move_cursor/4]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    doc_id, 
    doc = crdt_core:new(),
    cursors = #{},
    clients = [],
    pid_users = #{}
}).

-record(editor_docs, {doc_id, content}).

%%%===================================================================
%%% Client API
%%%===================================================================

start_link(DocId) ->
    gen_server:start_link({global, {doc, DocId}}, ?MODULE, [DocId], []).

join(DocId, Pid) ->
    gen_server:cast({global, {doc, DocId}}, {join, Pid}).

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
    {ok, #state{doc_id = DocId, doc = InitialDoc, cursors = #{}, pid_users = #{}}}.

handle_call(get_text, _From, State) ->
    Text = crdt_core:to_string(State#state.doc),
    {reply, Text, State}.

handle_cast({join, Pid}, State) ->
    erlang:monitor(process, Pid),
    CursorList = maps:to_list(State#state.cursors),
    Pid ! {sync_state, State#state.doc, CursorList},
    {noreply, State#state{clients = [Pid | State#state.clients]}};

handle_cast({insert, SenderPid, Id, UserId, Char}, State) ->
    NewPidUsers = maps:put(SenderPid, UserId, State#state.pid_users),
    NewDoc = crdt_core:insert(State#state.doc, Id, Char),
    broadcast(State#state.clients, SenderPid, {insert, Id, UserId, Char}),
    {noreply, State#state{doc = NewDoc, pid_users = NewPidUsers}};

handle_cast({delete, SenderPid, Id, UserId}, State) ->
    NewPidUsers = maps:put(SenderPid, UserId, State#state.pid_users),
    NewDoc = crdt_core:delete(State#state.doc, Id),
    broadcast(State#state.clients, SenderPid, {delete, Id, UserId}),
    {noreply, State#state{doc = NewDoc, pid_users = NewPidUsers}};

handle_cast({move, SenderPid, UserId, Pos}, State) ->
    NewPidUsers = maps:put(SenderPid, UserId, State#state.pid_users),
    NewCursors = maps:put(UserId, Pos, State#state.cursors),
    broadcast(State#state.clients, SenderPid, {move, UserId, Pos}),
    {noreply, State#state{cursors = NewCursors, pid_users = NewPidUsers}}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% 1. Find which user belongs to this Pid
    {UserToRemove, NewPidUsers} = case maps:find(Pid, State#state.pid_users) of
        {ok, User} -> {User, maps:remove(Pid, State#state.pid_users)};
        error -> {undefined, State#state.pid_users}
    end,

    %% 2. Remove their cursor and notify others
    NewCursors = case UserToRemove of
        undefined -> 
            State#state.cursors;
        _ ->
            broadcast(State#state.clients, Pid, {remove_cursor, UserToRemove}),
            maps:remove(UserToRemove, State#state.cursors)
    end,

    %% 3. Clean up client list
    NewClients = lists:delete(Pid, State#state.clients),
    
    case NewClients of
        [] ->
            mnesia:dirty_write(#editor_docs{
                doc_id = State#state.doc_id, 
                content = State#state.doc
            }),
            {stop, normal, State#state{clients = [], cursors = NewCursors, pid_users = NewPidUsers}};
        _ ->
            {noreply, State#state{clients = NewClients, cursors = NewCursors, pid_users = NewPidUsers}}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

broadcast(Clients, SenderPid, Msg) ->
    lists:foreach(fun(Pid) -> 
        if Pid =/= SenderPid -> Pid ! Msg; true -> ok end 
    end, Clients).