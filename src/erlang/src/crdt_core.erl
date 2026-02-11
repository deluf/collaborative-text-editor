-module(crdt_core).
-export([new/0, insert/3, delete/2, to_string/1]).

%% Example State: 
%% [
%%   {"123445", "H"},
%%   {"234343", "i"}
%% ]

new() ->
    [].

insert(Doc, ID, Char) ->
    lists:sort([{ID, Char} | Doc]).

delete(Doc, ID) ->
    lists:keydelete(ID, 1, Doc).

to_string(Doc) ->
    [Char || {_ID, Char} <- Doc].