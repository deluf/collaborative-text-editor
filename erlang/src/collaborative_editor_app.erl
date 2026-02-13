-module(collaborative_editor_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% 1. Connect to other nodes (Clustering)
    %%    We must do this BEFORE Mnesia starts so we can see remote tables.
    connect_to_nodes(),

    %% 2. Initialize Mnesia (Schema & Tables)
    init_mnesia(),

    %% 3. Get Port from config or default to 8086
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

stop(_State) ->
    ok.

%% ===================================================================
%% Internal Functions
%% ===================================================================

connect_to_nodes() ->
    %% Get list of nodes to join from env, e.g., ['node1@localhost', 'node2@localhost']
    Nodes = application:get_env(collaborative_editor, join_nodes, []),
    lists:foreach(fun(Node) -> 
        io:format("Attempting to join cluster node: ~p... ", [Node]),
        case net_adm:ping(Node) of
            pong -> 
                io:format("Success.~n"),
                mnesia:change_config(extra_db_nodes, [Node]);
            pang -> io:format("Failed (pang).~n")
        end
    end, Nodes).

init_mnesia() ->
    %% 1. Ensure the Schema itself is stored on disk.
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, schema, _, _}} -> ok;
        {aborted, Reason} -> error(Reason)
    end,

    %% 2. Create the table OR add a local copy if it exists remotely
    case mnesia:create_table(editor_docs, [
        {attributes, [doc_id, content]}, 
        {disc_copies, [node()]} 
    ]) of
        {atomic, ok} -> 
            ok; %% Created fresh on this node
        {aborted, {already_exists, editor_docs}} -> 
            io:format("Table 'editor_docs' exists remotely. Creating local replica...~n"),
            case mnesia:add_table_copy(editor_docs, node(), disc_copies) of
                {atomic, ok} -> ok;
                {aborted, {already_exists, editor_docs, _}} -> ok; %% We already have a copy
                {aborted, Error} -> error(Error)
            end;
        {aborted, Error} -> 
            error(Error)
    end,
    
    %% 3. Wait for tables
    mnesia:wait_for_tables([editor_docs], infinity).