-module(treewalker_crawler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_CODE, 200).
-define(A_BODY, <<"a body">>).
-define(SOME_AGENT_RULES, #{}).
-define(AN_ID, <<"an id">>).
-define(TIMEOUT, 1000).
-define(AN_URL, <<"http://www.anurl.com">>).

all() ->
    [
     retry_fetching_robots_policies_on_request_fail,
     retry_fetching_robots_policies_on_parsing_failure,
     can_fetch_a_page
    ].

init_per_suite(Config) ->
    application:set_env(treewalker, retry_interval, 100),
    {ok, Applications} = application:ensure_all_started(gproc),
    meck:new(treewalker_fetcher, [no_link]),
    meck:new(treewalker_link_filter, [no_link]),
    meck:new(treewalker_scraper, [no_link]),
    meck:new(treewalker_dispatcher, [no_link]),
    meck:new(robots, [no_link]),
    [{applications, Applications} | Config].

end_per_suite(Config) ->
    ok = lists:foreach(fun application:stop/1, ?config(applications, Config)),
    meck:unload(),
    Config.

init_per_testcase(_Name, Config) ->
    meck:reset(treewalker_fetcher),
    meck:reset(treewalker_link_filter),
    meck:reset(treewalker_scraper),
    meck:reset(treewalker_dispatcher),
    meck:reset(robots),

    RequestId = make_ref(),
    meck:expect(treewalker_dispatcher, request, fun (_, _) -> RequestId end),
    meck:expect(treewalker_fetcher, request, fun (_, _, _) -> {ok, {?A_CODE, ?A_BODY}} end),
    meck:expect(treewalker_scraper, scrap_links, fun (_, _, _) -> {ok, [?AN_URL]} end),
    meck:expect(treewalker_link_filter, filter, fun (_) -> true end),
    meck:expect(robots, parse, fun (_, _) -> {ok, ?SOME_AGENT_RULES} end),
    meck:expect(robots, is_allowed, fun (_, _, _) -> true end),
    [{request_id, RequestId} | Config].

end_per_testcase(_Name, Config) ->
    Config.

retry_fetching_robots_policies_on_request_fail() ->
    [{doc, "Given a failure, when fetching robots policies, then retries."}].
retry_fetching_robots_policies_on_request_fail(_Config) ->
    meck:expect(treewalker_fetcher, request, fun (_, _, _) -> {error, an_error} end),
    CrawlerConfig = treewalker_crawler_config:init(),
    {ok, _} = treewalker_crawler:start_link(?AN_ID, ?AN_ID, CrawlerConfig),

    ok = treewalker_crawler:start_crawler(?AN_ID),

    meck:wait(2, treewalker_fetcher, request, ['_', '_', '_'], ?TIMEOUT).

retry_fetching_robots_policies_on_parsing_failure() ->
    [{doc, "Given a parse error, when fetching robots policies, then retries."}].
retry_fetching_robots_policies_on_parsing_failure(_Config) ->
    meck:expect(robots, parse, fun (_, _) -> {error, an_error} end),
    CrawlerConfig = treewalker_crawler_config:init(),
    {ok, _} = treewalker_crawler:start_link(?AN_ID, ?AN_ID, CrawlerConfig),

    ok = treewalker_crawler:start_crawler(?AN_ID),

    meck:wait(2, robots, parse, ['_', '_'], ?TIMEOUT).

can_fetch_a_page() ->
    [{doc, "Given a valid URL, when requesting URL, then returns response."}].
can_fetch_a_page(Config) ->
    CrawlerConfig = treewalker_crawler_config:init(),
    RequestId = ?config(request_id, Config),
    {ok, Pid} = treewalker_crawler:start_link(?AN_ID, ?AN_ID, CrawlerConfig),
    ok = treewalker_crawler:start_crawler(?AN_ID),
    meck:wait(robots, parse, ['_', '_'], ?TIMEOUT),

    Pid ! {treewalker_dispatcher, RequestId, {ok, {?A_CODE, ?A_BODY}}},

    meck:wait(treewalker_scraper, scrap_links, ['_', '_', '_'], ?TIMEOUT).

%%%===================================================================
%%% Internal functions
%%%===================================================================
