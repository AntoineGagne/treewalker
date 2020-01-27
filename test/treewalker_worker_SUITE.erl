-module(treewalker_worker_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("src/treewalker.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_FETCHER, a_fetcher).
-define(A_USER_AGENT, <<"user-agent">>).
-define(A_BODY, <<"a body">>).
-define(AN_URL, <<"http://anurl.com">>).
-define(TIMEOUT, 1000).

all() ->
    [
     retry_on_failed_request,
     retry_on_crashed_request,
     can_successfully_request,
     stop_after_exceeded_retry_count
    ].

init_per_suite(Config) ->
    AConfig = treewalker_crawler_config:init(),
    AConfig2 = treewalker_crawler_config:fetcher(?A_FETCHER, AConfig),
    AConfig3 = treewalker_crawler_config:user_agent(?A_USER_AGENT, AConfig2),
    RetryPolicy = #retry_policy{min_delay = 100, max_delay = 200, max_retry = 1},

    meck:new(?A_FETCHER, [no_link, non_strict]),
    meck:new(backoff, [no_link, passthrough]),
    [{a_config, AConfig3}, {retry_policy, RetryPolicy} | Config].

end_per_suite(Config) ->
    meck:unload(),
    Config.

init_per_testcase(_Name, Config) ->
    meck:reset(?A_FETCHER),
    meck:reset(backoff),

    meck:expect(?A_FETCHER, request, fun (_, _, _) -> {ok, ?A_BODY} end),
    Config.

end_per_testcase(_Name, Config) ->
    Config.

retry_on_failed_request() ->
    [{doc, "Given a failed request, when requesting, then retries the request."}].
retry_on_failed_request(Config) ->
    AConfig = ?config(a_config, Config),
    RetryPolicy = ?config(retry_policy, Config),
    meck:expect(?A_FETCHER, request, [{['_', '_', '_'], meck:seq([{error, an_error},
                                                                  {ok, ?A_BODY}])}]),

    {ok, Pid} = treewalker_worker:start_link(?AN_URL, RetryPolicy, AConfig, self()),
    unlink(Pid),

    wait_for_message(Pid, fun (Result) -> ?assertMatch({ok, ?A_BODY}, Result) end).

retry_on_crashed_request() ->
    [{doc, "Given a crashed request, when requesting, then retries the request."}].
retry_on_crashed_request(Config) ->
    AConfig = ?config(a_config, Config),
    RetryPolicy = ?config(retry_policy, Config),
    meck:expect(?A_FETCHER, request, [{['_', '_', '_'], meck:seq([meck:raise(throw, an_error),
                                                                  {ok, ?A_BODY}])}]),

    {ok, Pid} = treewalker_worker:start_link(?AN_URL, RetryPolicy, AConfig, self()),
    unlink(Pid),

    wait_for_message(Pid, fun (Result) -> ?assertMatch({ok, ?A_BODY}, Result) end).

can_successfully_request() ->
    [{doc, "When requesting, then returns request's result."}].
can_successfully_request(Config) ->
    AConfig = ?config(a_config, Config),
    RetryPolicy = ?config(retry_policy, Config),

    {ok, Pid} = treewalker_worker:start_link(?AN_URL, RetryPolicy, AConfig, self()),
    unlink(Pid),

    wait_for_message(Pid, fun (Result) -> ?assertMatch({ok, ?A_BODY}, Result) end).

stop_after_exceeded_retry_count() ->
    [{doc, "Given an exceeded number of retries, when requesting, then stops requesting."}].
stop_after_exceeded_retry_count(Config) ->
    AConfig = ?config(a_config, Config),
    RetryPolicy = ?config(retry_policy, Config),
    meck:expect(?A_FETCHER, request, [{['_', '_', '_'], meck:seq([meck:raise(throw, an_error),
                                                                  {error, error}])}]),

    {ok, Pid} = treewalker_worker:start_link(?AN_URL, RetryPolicy, AConfig, self()),
    unlink(Pid),

    wait_for_message(Pid,
                     fun (Result) -> ?assertMatch({error, maximum_retries_reached}, Result) end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

wait_for_message(Pid, Expectation) ->
    receive
        {treewalker_worker, Pid, ?AN_URL, Result} ->
            Expectation(Result);
        Unexpected ->
            ct:fail("Unexpected message: ~p", [Unexpected])
    after
        ?TIMEOUT ->
            ct:fail("No message received.")
    end.
