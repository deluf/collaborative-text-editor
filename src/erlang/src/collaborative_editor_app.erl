-module(collaborative_editor_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Define the Routes
    %% This matches the binding 'doc_id' used in ws_handler:init/2
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws/:doc_id", ws_handler, []}
        ]}
    ]),

    %% Start Cowboy HTTP listener on port 8080
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    %% Start the default supervisor
    collaborative_editor_sup:start_link().

stop(_State) ->
    ok.