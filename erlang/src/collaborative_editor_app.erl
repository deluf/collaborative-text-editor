-module(collaborative_editor_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc Application start callback.
%% Initializes clustering, Mnesia schema/tables, and starts the Cowboy HTTP listener.
-spec start(application:start_type(), any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    connect_to_nodes(),
    init_mnesia(),

    Port = application:get_env(collaborative_editor, http_port, 8086),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/:doc_id", ws_handler, []}
        ]}
    ]),

    io:format("Starting server on port ~p...~n", [Port]),

    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    collaborative_editor_sup:start_link().

%% @doc Application stop callback.
-spec stop(any()) -> ok.
stop(_State) ->
    ok.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc Attempts to connect to nodes defined in the 'join_nodes' environment variable.
%% Necessary for establishing the distributed Erlang cluster before Mnesia starts.
-spec connect_to_nodes() -> ok.
connect_to_nodes() ->
    Nodes = application:get_env(collaborative_editor, join_nodes, []),
    lists:foreach(fun(Node) -> 
        io:format("Attempting to join cluster node: ~p... ", [Node]),
        case net_adm:ping(Node) of
            pong -> 
                io:format("Success.~n"),
                mnesia:change_config(extra_db_nodes, [Node]);
            pang -> 
                io:format("Failed (pang).~n")
        end
    end, Nodes).

%% @doc Initializes the Mnesia database.
%% Ensures the schema is on disk and the 'editor_docs' table exists locally or is replicated.
-spec init_mnesia() -> ok.
init_mnesia() ->
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, schema, _, _}} -> ok;
        {aborted, Reason} -> error(Reason)
    end,

    case mnesia:create_table(editor_docs, [
        {attributes, [doc_id, content]}, 
        {disc_copies, [node()]} 
    ]) of
        {atomic, ok} -> 
            ok;
        {aborted, {already_exists, editor_docs}} -> 
            io:format("Table 'editor_docs' exists remotely. Creating local replica...~n"),
            case mnesia:add_table_copy(editor_docs, node(), disc_copies) of
                {atomic, ok} -> ok;
                {aborted, {already_exists, editor_docs, _}} -> ok;
                {aborted, Error} -> error(Error)
            end;
        {aborted, Error} -> 
            error(Error)
    end,
    
    mnesia:wait_for_tables([editor_docs], infinity).