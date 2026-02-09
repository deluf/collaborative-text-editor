-module(ws_handler).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

websocket_init(State) ->
    %% In a real app, you would parse the DocId from the URL or request.
    %% For now, we simulate different docs for testing.
    DocId = "meeting_notes", 

    %% 1. Ask the registry to find or create the server for this DocId
    doc_registry:get_server(DocId),

    %% 2. Join the global session
    doc_server:join(DocId, self()),
    
    %% Store DocId in state so we know where to route messages later
    {ok, maps:put(doc_id, DocId, State)}.

websocket_handle({text, Json}, State) ->
    DocId = maps:get(doc_id, State),
    Map = jsx:decode(Json, [return_maps]),
    
    case maps:get(<<"type">>, Map) of
        <<"insert">> ->
            Char = maps:get(<<"char">>, Map),
            Pos = maps:get(<<"pos">>, Map),
            User = maps:get(<<"user">>, Map),
            doc_server:add_char(DocId, Pos, User, Char);
            
        <<"delete">> ->
            Pos = maps:get(<<"pos">>, Map),
            User = maps:get(<<"user">>, Map),
            doc_server:remove_char(DocId, Pos, User)
    end,
    {ok, State}.

%% The rest of the file (websocket_info) remains exactly the same as your original.
websocket_info(Msg, State) ->
    %% (Copy the websocket_info functions from your original upload here)
    handle_info_logic(Msg, State).

%% Hidden helper just to keep this snippet short
handle_info_logic({insert, {Pos, User}, Char}, State) ->
    Json = jsx:encode(#{type => <<"insert">>, pos => Pos, user => User, char => Char}),
    {reply, {text, Json}, State};
handle_info_logic({delete, {Pos, User}}, State) ->
    Json = jsx:encode(#{type => <<"delete">>, pos => Pos, user => User}),
    {reply, {text, Json}, State};
handle_info_logic({sync_state, Doc}, State) ->
    JsonList = [ #{pos => P, user => U, char => C} || {{P, U}, C} <- Doc ],
    Json = jsx:encode(#{type => <<"sync">>, data => JsonList}),
    {reply, {text, Json}, State}.