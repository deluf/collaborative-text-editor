-module(collaborative_editor_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% 1. Initialize Mnesia
    init_mnesia(),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/:doc_id", ws_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8086}],
        #{env => #{dispatch => Dispatch}}
    ),

    collaborative_editor_sup:start_link().

stop(_State) ->
    ok.

%% collaborative_editor_app.erl

init_mnesia() ->
    %% 1. Ensure the Schema itself is stored on disk.
    %%    Since Mnesia is already running (ram_copies by default on first run),
    %%    we change the schema table to disc_copies. This persists the DB.
    case mnesia:change_table_copy_type(schema, node(), disc_copies) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, schema, _Node, _Type}} -> ok; %% Already on disk
        {aborted, Reason} -> error(Reason)
    end,

    %% 2. Create the table (if it doesn't exist)
    case mnesia:create_table(editor_docs, [
        {attributes, [doc_id, content]}, 
        {disc_copies, [node()]} 
    ]) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, editor_docs}} -> ok; %% Table already exists
        {aborted, Error} -> error(Error)
    end,
    
    %% 3. Wait for tables to be accessible
    mnesia:wait_for_tables([editor_docs], 5000).