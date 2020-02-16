-module(treewalker_rate_limit).

%% API
-export([delay/1]).

-define(MILLISECONDS_PER_SECOND, 1000).

%%%===================================================================
%%% API
%%%===================================================================

-spec delay(MaxDelayInSeconds :: pos_integer()) -> ComputedDelayInMilliseconds :: pos_integer().
delay(MaxDelayInSeconds) ->
    rand:uniform(MaxDelayInSeconds) * ?MILLISECONDS_PER_SECOND.

%%%===================================================================
%%% Internal functions
%%%===================================================================
