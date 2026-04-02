-module(doc_sweeper).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SWEEP_INTERVAL, 60000). % process runs every 60 seconds
-define(IDLE_TIMEOUT, 60000).   % idle time before deletion

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the document sweeper process and registers it locally.
%% This function is typically called by the application's supervision tree.
%% @returns {ok, pid()} | {error, term()}
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% GenServer Callbacks
%%%===================================================================

%% @private
%% @doc Initializes the sweeper. Runs an immediate sweep at startup
%% and schedules the first periodic sweep.
init([]) ->
    sweep_stale_docs(),
    erlang:send_after(?SWEEP_INTERVAL, self(), trigger_sweep),
    {ok, #{}}.

%% @private
%% @doc Handles the `trigger_sweep` message to execute the database cleanup,
%% then reschedules the next sweep.
handle_info(trigger_sweep, State) ->
    sweep_stale_docs(),
    erlang:send_after(?SWEEP_INTERVAL, self(), trigger_sweep),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

%% @private
handle_call(_Request, _From, State) -> 
    {reply, ok, State}.

%% @private
handle_cast(_Msg, State) -> 
    {noreply, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
%% @doc Identifies and safely deletes stale documents from the `editor_docs`
%% Mnesia table. Extracts the `doc_id` using dirty reads to prevent table 
%% locks, and then applies individual atomic transactions to delete them.
sweep_stale_docs() ->
    Now = erlang:system_time(millisecond),
    Threshold = Now - ?IDLE_TIMEOUT,

    %% A Match Specification defining what we want to find.
    %% Structure: {RecordTuple, [Conditions], [ReturnValues]}
    %% We match the 4-tuple Mnesia stores: {editor_docs, DocId, Content, LastActive}
    %% '$1' is the DocId, '_' ignores Content, '$2' is LastActive.
    MatchSpec = [{
        {editor_docs, '$1', '_', '$2'},
        %% Conditions: LastActive must be an integer AND strictly less than Threshold
        [{is_integer, '$2'}, {'<', '$2', Threshold}],
        %% Return Value: Just return the DocId ('$1')
        ['$1']
    }],

    %% Execute a dirty read to find stale IDs without locking the table
    StaleDocIds = mnesia:dirty_select(editor_docs, MatchSpec),

    %% Iterate through the resulting list of IDs and delete them
    lists:foreach(fun(DocId) ->
        io:format("Sweeping stale document: ~p~n", [DocId]),
        %% We wrap the deletion in a small transaction to ensure atomic removal
        mnesia:transaction(fun() -> mnesia:delete({editor_docs, DocId}) end)
    end, StaleDocIds),

    io:format("Sweep complete. Removed ~p stale documents.~n", [length(StaleDocIds)]).