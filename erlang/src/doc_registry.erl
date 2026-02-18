-module(doc_registry).
-export([get_server/1]).

%% @doc Retrieves the PID of a doc_server for a specific Document ID.
%% If the server does not exist, it spawns a new one and registers it globally.
%% Handles race conditions where two nodes might attempt to start the same doc simultaneously.
%% @param DocId The unique identifier of the document.
%% @returns The Pid of the document process.
-spec get_server(term()) -> pid().
get_server(DocId) ->
    GlobalName = {doc, DocId},
    case global:whereis_name(GlobalName) of
        undefined ->
            case doc_server:start_link(DocId) of
                {ok, Pid} -> Pid;
                {error, {already_started, Pid}} -> Pid
            end;
        Pid -> 
            Pid
    end.