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
            %% Table exists (e.g., on another node). 
            %% Request a local disk replica so we persist it here too.
            case mnesia:add_table_copy(editor_docs, node(), disc_copies) of
                {atomic, ok} -> ok;
                {aborted, {already_exists, editor_docs, _}} -> ok; %% We already have a copy
                {aborted, Error} -> error(Error)
            end;
        {aborted, Error} -> 
            error(Error)
    end,
    
    %% 3. Wait for tables (infinity is safer for clustering startup)
    mnesia:wait_for_tables([editor_docs], infinity).