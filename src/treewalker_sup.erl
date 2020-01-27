-module(treewalker_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {#{strategy => one_for_all,
            intensity => 0,
            period => 1},
          [#{id => treewalker_crawlers_sup,
             start => {treewalker_crawlers_sup, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [treewalker_crawlers_sup]}]}}.
