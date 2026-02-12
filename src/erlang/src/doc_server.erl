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
    clients = []
}).

-record(editor_docs, {doc_id, content}).

%%% Client API %%%

start_link(DocId) ->
    gen_server:start_link({global, {doc, DocId}}, ?MODULE, [DocId], []).

join(DocId, ClientPid) ->
    gen_server:cast({global, {doc, DocId}}, {join, ClientPid}).

add_char(DocId, SenderPid, Id, UserId, Char) ->
    gen_server:cast({global, {doc, DocId}}, {insert, SenderPid, Id, UserId, Char}).

remove_char(DocId, SenderPid, Id, UserId) ->
    gen_server:cast({global, {doc, DocId}}, {delete, SenderPid, Id, UserId}).

move_cursor(DocId, SenderPid, UserId, Pos) ->
    gen_server:cast({global, {doc, DocId}}, {move, SenderPid, UserId, Pos}).

get_text(DocId) ->
    gen_server:call({global, {doc, DocId}}, get_text).

%%% GenServer Callbacks %%%
init([DocId]) ->
    InitialDoc = case mnesia:dirty_read(editor_docs, DocId) of
        [] -> crdt_core:new();
        [#editor_docs{content = SavedDoc}] -> SavedDoc
    end,
    {ok, #state{doc_id = DocId, doc = InitialDoc}}.

handle_call(get_text, _From, State) ->
    Text = crdt_core:to_string(State#state.doc),
    {reply, Text, State}.

handle_cast({join, Pid}, State) ->
    erlang:monitor(process, Pid),
    Pid ! {sync_state, State#state.doc},
    {noreply, State#state{clients = [Pid | State#state.clients]}};

handle_cast({insert, SenderPid, Id, UserId, Char}, State) ->
    NewDoc = crdt_core:insert(State#state.doc, Id, Char),
    broadcast(State#state.clients, SenderPid, {insert, Id, UserId, Char}),
    {noreply, State#state{doc = NewDoc}};

handle_cast({delete, SenderPid, Id, UserId}, State) ->
    NewDoc = crdt_core:delete(State#state.doc, Id),
    broadcast(State#state.clients, SenderPid, {delete, Id, UserId}),
    {noreply, State#state{doc = NewDoc}};

handle_cast({move, SenderPid, UserId, Pos}, State) ->
    broadcast(State#state.clients, SenderPid, {move, UserId, Pos}),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    NewClients = lists:delete(Pid, State#state.clients),
    case NewClients of
        [] ->
            mnesia:dirty_write(#editor_docs{
                doc_id = State#state.doc_id, 
                content = State#state.doc
            }),
            {stop, normal, State#state{clients = []}};
        _ ->
            {noreply, State#state{clients = NewClients}}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

broadcast(Clients, SenderPid, Msg) ->
    lists:foreach(fun(Pid) -> 
        if 
            Pid =/= SenderPid -> Pid ! Msg;
            true -> ok
        end 
    end, Clients).