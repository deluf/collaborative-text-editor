-module(doc_server).
-behaviour(gen_server).

%% API
-export([start_link/1, join/2, add_char/4, remove_char/3, get_text/1]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    doc = crdt_core:new(),
    clients = []
}).

%%% Client API %%%

start_link(DocId) ->
    gen_server:start_link({global, {doc, DocId}}, ?MODULE, [], []).

join(DocId, ClientPid) ->
    gen_server:cast({global, {doc, DocId}}, {join, ClientPid}).

add_char(DocId, ID, UserId, Char) ->
    gen_server:cast({global, {doc, DocId}}, {insert, ID, UserId, Char}).

remove_char(DocId, ID, UserId) ->
    gen_server:cast({global, {doc, DocId}}, {delete, ID, UserId}).

get_text(DocId) ->
    gen_server:call({global, {doc, DocId}}, get_text).

%%% GenServer Callbacks %%%

init([]) ->
    {ok, #state{}}.

handle_call(get_text, _From, State) ->
    Text = crdt_core:to_string(State#state.doc),
    {reply, Text, State}.

handle_cast({join, Pid}, State) ->
    erlang:monitor(process, Pid),
    Pid ! {sync_state, State#state.doc},
    {noreply, State#state{clients = [Pid | State#state.clients]}};

handle_cast({insert, ID, UserId, Char}, State) ->
    NewDoc = crdt_core:insert(State#state.doc, ID, Char),
    broadcast(State#state.clients, {insert, ID, UserId, Char}),
    {noreply, State#state{doc = NewDoc}};

handle_cast({delete, ID, UserId}, State) ->
    NewDoc = crdt_core:delete(State#state.doc, ID),
    broadcast(State#state.clients, {delete, ID, UserId}),
    {noreply, State#state{doc = NewDoc}}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    NewClients = lists:delete(Pid, State#state.clients),
    {noreply, State#state{clients = NewClients}};
handle_info(_Msg, State) ->
    {noreply, State}.

broadcast(Clients, Msg) ->
    lists:foreach(fun(Pid) -> Pid ! Msg end, Clients).