-module(doc_registry).
-export([get_server/1]).

%% @doc Returns the PID of a doc_server. 
%% If it doesn't exist, it spawns one and registers it globally.
get_server(DocId) ->
    GlobalName = {doc, DocId},
    %% Check if the process exists in the global registry
    case global:whereis_name(GlobalName) of
        undefined ->
            %% Attempt to start it.
            %% If two nodes race to start the same doc, doc_server:start_link 
            %% will return {error, {already_started, Pid}} for the loser.
            case doc_server:start_link(DocId) of
                {ok, Pid} -> Pid;
                {error, {already_started, Pid}} -> Pid
            end;
        Pid -> 
            Pid
    end.