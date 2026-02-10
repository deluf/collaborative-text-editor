-module(ws_handler).
-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

%% ===================================================================
%% 1. HTTP HANDSHAKE (init/2)
%% ===================================================================
%% This is called before the WebSocket upgrade. 
%% We extract the DocId from the URL here (e.g., /ws/meeting_notes).
init(Req, _Opts) ->
    %% Extract the :doc_id binding from the router path
    %% Note: This returns a binary, e.g., <<"meeting_notes">>
    DocId = cowboy_req:binding(doc_id, Req),

    %% Initialize our state map with the DocId
    State = #{doc_id => DocId},

    %% Upgrade the connection to a WebSocket
    {cowboy_websocket, Req, State}.


%% ===================================================================
%% 2. WEBSOCKET INITIALIZATION
%% ===================================================================
websocket_init(State) ->
    %% Retrieve the DocId we stored in init/2
    DocId = maps:get(doc_id, State),

    %% 1. Ask the registry to find or create the server for this DocId
    doc_registry:get_server(DocId),

    %% 2. Join the global session so this process receives updates
    doc_server:join(DocId, self()),

    %% We don't need to modify State, it already has doc_id
    {ok, State}.


%% ===================================================================
%% 3. INCOMING MESSAGES (From Client Browser)
%% ===================================================================
websocket_handle({text, Json}, State) ->
    DocId = maps:get(doc_id, State),
    
    %% Decode JSON to a map
    Map = jsx:decode(Json, [return_maps]),

    %% Check the "type" field to decide what to do
    case maps:get(<<"type">>, Map) of
        
        <<"insert">> ->
            Char = maps:get(<<"char">>, Map),
            Pos  = maps:get(<<"pos">>, Map),
            User = maps:get(<<"user">>, Map),
            doc_server:add_char(DocId, Pos, User, Char);

        <<"delete">> ->
            Pos  = maps:get(<<"pos">>, Map),
            User = maps:get(<<"user">>, Map),
            doc_server:remove_char(DocId, Pos, User);

        _ -> 
            %% Ignore unknown message types
            ok
    end,
    {ok, State};

%% Handle ping/pong or other frame types if necessary
websocket_handle(_Data, State) ->
    {ok, State}.


%% ===================================================================
%% 4. OUTGOING MESSAGES (From Erlang Processes)
%% ===================================================================
%% These messages are sent by doc_server to this process (self())

%% Case 1: Another user inserted a character
websocket_info({insert, {Pos, User}, Char}, State) ->
    Resp = #{
        type => <<"insert">>,
        pos  => Pos, 
        user => User, 
        char => Char
    },
    Json = jsx:encode(Resp),
    {reply, {text, Json}, State};

%% Case 2: Another user deleted a character
websocket_info({delete, {Pos, User}}, State) ->
    Resp = #{
        type => <<"delete">>, 
        pos  => Pos, 
        user => User
    },
    Json = jsx:encode(Resp),
    {reply, {text, Json}, State};

%% Case 3: Initial sync when joining
websocket_info({sync_state, Doc}, State) ->
    %% Convert the internal doc representation to a list of JSON objects
    JsonList = [ #{pos => P, user => U, char => C} || {{P, U}, C} <- Doc ],
    
    Resp = #{
        type => <<"sync">>, 
        data => JsonList
    },
    Json = jsx:encode(Resp),
    {reply, {text, Json}, State};

%% Catch-all for any other Erlang messages
websocket_info(_Info, State) ->
    {ok, State}.