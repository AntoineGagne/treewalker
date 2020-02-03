-module(treewalker_app).

-behaviour(application).

-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

start(_StartType, _StartArgs) ->
    treewalker_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
