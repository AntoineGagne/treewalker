-module(treewalker_dispatcher_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(AN_ID, <<"an id">>).
-define(AN_URL, <<"an url">>).
-define(A_BODY, <<"a body">>).
-define(TIMEOUT, 1000).

all() ->
    [
     send_response_on_start_failure,
     send_response_on_worker_response,
     send_response_on_worker_crash
    ].

init_per_suite(Config) ->
    application:set_env(treewalker, min_retry_delay, 100),
    application:set_env(treewalker, max_retry_delay, 200),
    application:set_env(treewalker, max_retries, 1),

    {ok, Applications} = application:ensure_all_started(gproc),
    meck:new(treewalker_worker, [no_link]),
    [{applications, Applications} | Config].

end_per_suite(Config) ->
    ok = lists:foreach(fun application:stop/1, ?config(applications, Config)),
    meck:unload(),
    Config.

init_per_testcase(_Name, Config) ->
    meck:reset(treewalker_worker),
    AConfig = treewalker_crawler_config:init(),

    {ok, Pid} = treewalker_dispatcher:start_link(?AN_ID, AConfig),
    meck:expect(treewalker_worker, start_link,
                fun (_, _, _, _) ->
                        WorkerPid = make_pid(),
                        Pid ! {treewalker_worker, WorkerPid, ?AN_URL, {ok, ?A_BODY}},
                        {ok, WorkerPid}
                end),
    Config.

end_per_testcase(_Name, Config) ->
    Config.

send_response_on_start_failure() ->
    [{doc, "Given an error, when starting worker, then responds with error."}].
send_response_on_start_failure(_Config) ->
    meck:expect(treewalker_worker, start_link, fun (_, _, _, _) -> {error, an_error} end),

    ok = treewalker_dispatcher:request(?AN_ID, ?AN_URL),

    wait_for_message(fun (Response) -> ?assertMatch({error, an_error}, Response) end).

send_response_on_worker_response() ->
    [{doc, "Given a worker response, when waiting for response, then sends response."}].
send_response_on_worker_response(_Config) ->
    ok = treewalker_dispatcher:request(?AN_ID, ?AN_URL),

    wait_for_message(fun (Response) -> ?assertMatch({ok, ?A_BODY}, Response) end).

send_response_on_worker_crash() ->
    [{doc, "Given a crashed worker, when waiting for response, then sends response."}].
send_response_on_worker_crash(_Config) ->
    WorkerPid = make_pid(),
    unlink(WorkerPid),
    meck:expect(treewalker_worker, start_link, fun (_, _, _, _) -> {ok, WorkerPid} end),

    ok = treewalker_dispatcher:request(?AN_ID, ?AN_URL),
    meck:wait(treewalker_worker, start_link, ['_', '_', '_', '_'], ?TIMEOUT),
    exit(WorkerPid, kill),

    wait_for_message(fun (Response) -> ?assertMatch({error, _}, Response) end).

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_pid() ->
    spawn_link(fun () -> timer:sleep(60000) end).

wait_for_message(Expectation) ->
    receive
        {treewalker_dispatcher, ?AN_URL, Result} ->
            Expectation(Result);
        Unexpected ->
            ct:fail("Unexpected message: ~p", [Unexpected])
    after
        ?TIMEOUT ->
            ct:fail("No message received.")
    end.
