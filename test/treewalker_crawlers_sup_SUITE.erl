-module(treewalker_crawlers_sup_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_NAME, <<"crawler">>).

all() ->
    [
     can_start_supervisor,
     can_remove_existing_crawler,
     return_error_on_non_existent_crawler,
     can_add_a_crawler
    ].

init_per_suite(Config) ->
    meck:new(treewalker_crawler_sup, [no_link]),
    meck:new(supervisor, [no_link, unstick, passthrough]),
    meck:new(gproc, [no_link]),
    Config.

end_per_suite(Config) ->
    meck:unload(),
    Config.

init_per_testcase(_Name, Config) ->
    meck:reset(supervisor),
    meck:reset(treewalker_crawler_sup),
    meck:reset(gproc),

    Pid = make_pid(),
    meck:expect(treewalker_crawler_sup, start_link, [{['_', '_', '_'], {ok, Pid}}]),
    meck:expect(gproc, where, [{['_'], Pid}]),
    [{pid, Pid} | Config].

end_per_testcase(_Name, Config) ->
    Config.

can_start_supervisor() ->
    [{doc, "When starting, then starts the supervisor."}].
can_start_supervisor(_Config) ->
    ?assertMatch({ok, _}, treewalker_crawlers_sup:start_link()).

can_remove_existing_crawler() ->
    [{doc, "Given an existing crawler, when removing, then removes crawler."}].
can_remove_existing_crawler(Config) ->
    Pid = ?config(pid, Config),
    meck:expect(supervisor, terminate_child, [{[treewalker_crawlers_sup, Pid], ok}]),

    ?assertMatch(ok, treewalker_crawlers_sup:remove_crawler(?A_NAME)).

return_error_on_non_existent_crawler() ->
    [{doc, "Given a non-existent crawler, when removing, then returns not found error."}].
return_error_on_non_existent_crawler(_Config) ->
    meck:expect(gproc, where, [{['_'], undefined}]),

    ?assertMatch({error, not_found}, treewalker_crawlers_sup:remove_crawler(?A_NAME)).

can_add_a_crawler() ->
    [{doc, "Given a valid configuration, when adding crawler, then adds crawler."}].
can_add_a_crawler(_Config) ->
    {ok, _} = treewalker_crawlers_sup:start_link(),

    ?assertMatch({ok, _}, treewalker_crawlers_sup:add_crawler(?A_NAME, #{})).

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_pid() ->
    spawn_link(fun Loop () ->
                       timer:sleep(60000),
                       Loop ()
               end).
