-module(doc_server).
-behaviour(gen_server).

%% API
-export([start_link/1, join/2, add_char/4, remove_char/3, move_cursor/3, get_text/1]).
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

add_char(DocId, Id, UserId, Char) ->
    gen_server:cast({global, {doc, DocId}}, {insert, Id, UserId, Char}).

remove_char(DocId, Id, UserId) ->
    gen_server:cast({global, {doc, DocId}}, {delete, Id, UserId}).

move_cursor(DocId, UserId, Pos) ->
    gen_server:cast({global, {doc, DocId}}, {move, UserId, Pos}).

get_text(DocId) ->
    gen_server:call({global, {doc, DocId}}, get_text).

%%% GenServer Callbacks %%%
init([DocId]) ->
    %% Load initial state from Mnesia (or create new)
    InitialDoc = case mnesia:dirty_read(editor_docs, DocId) of
        [] -> 
            crdt_core:new();
        [#editor_docs{content = SavedDoc}] -> 
            SavedDoc
    end,
    {ok, #state{doc_id = DocId, doc = InitialDoc}}.

handle_call(get_text, _From, State) ->
    Text = crdt_core:to_string(State#state.doc),
    {reply, Text, State}.

handle_cast({join, Pid}, State) ->
    erlang:monitor(process, Pid),
    Pid ! {sync_state, State#state.doc},
    {noreply, State#state{clients = [Pid | State#state.clients]}};

handle_cast({insert, Id, UserId, Char}, State) ->
    NewDoc = crdt_core:insert(State#state.doc, Id, Char),
    broadcast(State#state.clients, {insert, Id, UserId, Char}),
    {noreply, State#state{doc = NewDoc}};

handle_cast({delete, Id, UserId}, State) ->
    NewDoc = crdt_core:delete(State#state.doc, Id),
    broadcast(State#state.clients, {delete, Id, UserId}),
    {noreply, State#state{doc = NewDoc}};

handle_cast({move, UserId, Pos}, State) ->
    broadcast(State#state.clients, {move, UserId, Pos}),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    NewClients = lists:delete(Pid, State#state.clients),
    
    case NewClients of
        [] ->
            %% No clients left. Save state and stop the server.
            mnesia:dirty_write(#editor_docs{
                doc_id = State#state.doc_id, 
                content = State#state.doc
            }),
            {stop, normal, State#state{clients = []}};
        _ ->
            %% Clients still connected, just update the list.
            {noreply, State#state{clients = NewClients}}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

broadcast(Clients, Msg) ->
    lists:foreach(fun(Pid) -> Pid ! Msg end, Clients).