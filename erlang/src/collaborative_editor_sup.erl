-module(collaborative_editor_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Flags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },
    % Not supervising anything here:
    % - WebSocket processes (ws_handler) are already owned and supervised by Cowboy
    % - Document processes (doc_server) are spawned on demand by doc_registry when the first user opens a document
    %   and they live as free processes. If a document process crash, nothing will restart it. The next user to open
    %   that document will just get a fresh one 
    Childs = [], 
    {ok, {Flags, Childs}}.
