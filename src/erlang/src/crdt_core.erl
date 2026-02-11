-module(crdt_core).
-export([new/0, insert/3, delete/2, to_string/1]).

%% The document is just a list of {ID, Char} tuples.
%% ID is a tuple: {PositionList, UserID} 
%% Example State: 
%% [
%%   {{[1], "userA"}, "H"},
%%   {{[1, 5], "userB"}, "i"}
%% ]

new() ->
    [].

%% Insert a character with a specific unique ID
%% We simply append and then sort. 
%% Because IDs are unique and sortable, this guarantees consistency.
insert(Doc, ID, Char) ->
    lists:sort([{ID, Char} | Doc]).

%% Delete a character by its exact ID
delete(Doc, ID) ->
    lists:keydelete(ID, 1, Doc).

%% Convert the CRDT structure back to a plain string for viewing
to_string(Doc) ->
    [Char || {_ID, Char} <- Doc].