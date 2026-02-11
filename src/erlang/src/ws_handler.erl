-module(ws_handler).
-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, _Opts) ->
    DocId = cowboy_req:binding(doc_id, Req),
    State = #{doc_id => DocId},
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    DocId = maps:get(doc_id, State),
    doc_registry:get_server(DocId),
    doc_server:join(DocId, self()),
    {ok, State}.

%% ===================================================================
%% 3. INCOMING MESSAGES (From Client Browser)
%% ===================================================================
websocket_handle({text, Json}, State) ->
    DocId = maps:get(doc_id, State),
    Map = jsx:decode(Json, [return_maps]),

    case maps:get(<<"type">>, Map) of
        <<"insert">> ->
            Char = maps:get(<<"char">>, Map),
            Pos  = maps:get(<<"pos">>, Map),  %% This is the ID
            User = maps:get(<<"user">>, Map),
            doc_server:add_char(DocId, Pos, User, Char);

        <<"delete">> ->
            Pos  = maps:get(<<"pos">>, Map),  %% This is the ID
            User = maps:get(<<"user">>, Map),
            doc_server:remove_char(DocId, Pos, User);
        _ -> 
            ok
    end,
    {ok, State};
websocket_handle(_Data, State) ->
    {ok, State}.

%% ===================================================================
%% 4. OUTGOING MESSAGES (From Erlang Processes)
%% ===================================================================

websocket_info({insert, Pos, User, Char}, State) ->
    Resp = #{
        type => <<"insert">>,
        pos  => Pos, 
        user => User, 
        char => Char
    },
    Json = jsx:encode(Resp),
    {reply, {text, Json}, State};

websocket_info({delete, Pos, User}, State) ->
    Resp = #{
        type => <<"delete">>, 
        pos  => Pos, 
        user => User
    },
    Json = jsx:encode(Resp),
    {reply, {text, Json}, State};

websocket_info({sync_state, Doc}, State) ->
    JsonList = [ #{pos => P, char => C} || {P, C} <- Doc ],
    
    Resp = #{
        type => <<"sync">>, 
        data => JsonList
    },
    Json = jsx:encode(Resp),
    {reply, {text, Json}, State};

websocket_info(_Info, State) ->
    {ok, State}.