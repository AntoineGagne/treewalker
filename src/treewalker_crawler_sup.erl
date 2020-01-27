-module(treewalker_crawler_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(DISPATCHER_ID(Id), {dispatcher, Id}).
-define(CRAWLER_ID(Id), {crawler, Id}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(term(), term(), map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Name, Id, Options) ->
    supervisor:start_link(?VIA_GPROC(Name), ?MODULE, [Id, Options]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Id, Options]) ->
    DispatcherId = ?DISPATCHER_ID(Id),
    CrawlerId = ?CRAWLER_ID(Id),
    {ok, {#{strategy => one_for_all,
            intensity => 5,
            period => 10},
          [
           #{id => treewalker_dispatcher,
             start => {treewalker_dispatcher, start_link, [DispatcherId, Options]},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => []},
           #{id => treewalker_crawler,
             start => {treewalker_crawler, start_link, [CrawlerId, Options]},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => []}
          ]}}.
