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

    try
        Map = jsx:decode(Json, [return_maps]),

        case maps:get(<<"action">>, Map, undefined) of
            <<"insert">> ->
                Char = maps:get(<<"char">>, Map),
                Id   = maps:get(<<"id">>, Map),
                User = maps:get(<<"username">>, Map),
                doc_server:add_char(DocId, self(), Id, User, Char);

            <<"delete">> ->
                Id   = maps:get(<<"id">>, Map),
                User = maps:get(<<"username">>, Map),
                doc_server:remove_char(DocId, self(), Id, User);

            <<"move">> ->
                Id   = maps:get(<<"id">>, Map),
                User = maps:get(<<"username">>, Map),
                doc_server:move_cursor(DocId, self(), User, Id);

            _ -> 
                ok
        end
    catch
        _:_ -> ok
    end,
    {ok, State};

websocket_handle(_Data, State) ->
    {ok, State}.

%% ===================================================================
%% 4. OUTGOING MESSAGES (From Erlang Processes)
%% ===================================================================

websocket_info({insert, Id, User, Char}, State) ->
    Resp = #{
        action => <<"insert">>,
        id  => Id, 
        username => User, 
        char => Char
    },
    {reply, {text, jsx:encode(Resp)}, State};

websocket_info({delete, Id, User}, State) ->
    Resp = #{
        action => <<"delete">>, 
        id  => Id, 
        username => User
    },
    {reply, {text, jsx:encode(Resp)}, State};

websocket_info({move, User, Id}, State) ->
    Resp = #{
        action => <<"move">>,
        username => User,
        id  => Id
    },
    {reply, {text, jsx:encode(Resp)}, State};

websocket_info({remove_cursor, User}, State) ->
    Resp = #{
        action => <<"disconnected">>,
        username => User
    },
    {reply, {text, jsx:encode(Resp)}, State};

websocket_info({sync_state, Doc, Cursors}, State) ->
    DocJson = [ #{id => P, char => C} || {P, C} <- lists:sort(Doc) ],
    CursorJson = [ #{username => U, id => P} || {U, P} <- Cursors ],
    Resp = #{
        action => <<"sync">>, 
        data => DocJson,
        cursors => CursorJson
    },
    {reply, {text, jsx:encode(Resp)}, State};

websocket_info(_Info, State) ->
    {ok, State}.