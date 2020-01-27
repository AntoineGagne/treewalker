-module(treewalker_crawlers_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_crawler/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SUP_ID(Id), {sup, {crawler, Id}}).
-define(USER_AGENT, <<"treewalker/1.0.0">>).

-type child() :: undefined | pid().

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_crawler(term(), map()) -> {ok, child()} | {ok, child(), term()} | {error, term()}.
start_crawler(Name, Custom) ->
    Config = compute_config(Custom),
    supervisor:start_child(?MODULE, [?SUP_ID(Name), Name, Config]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 5,
            period => 10},
          [#{id => treewalker_crawler_sup,
             start => {treewalker_crawler_sup, start_link, []},
             restart => temporary,
             shutdown => 5000,
             type => supervisor,
             modules => [treewalker_crawler_sup]}]}}.

compute_config(Custom) ->
    UserAgent = application:get_env(treewalker, user_agent, ?USER_AGENT),
    Default = treewalker_crawler_config:init(),
    Default2 = treewalker_crawler_config:user_agent(UserAgent, Default),
    treewalker_crawler_config:merge(Default2, Custom).
