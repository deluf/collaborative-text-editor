-module(crdt_core).
-export([new/0, insert/3, delete/2, to_string/1]).

new() ->
    [].

insert(Doc, ID, Char) ->
    [{ID, Char} | Doc].

delete(Doc, ID) ->
    lists:keydelete(ID, 1, Doc).

to_string(Doc) ->
    [Char || {_ID, Char} <- lists:sort(Doc)].