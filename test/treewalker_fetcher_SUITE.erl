-module(treewalker_fetcher_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_CODE, 200).
-define(A_BODY, <<"a body">>).
-define(A_USER_AGENT, <<"user-agent">>).
-define(AN_URL, <<"http://anurl.com">>).

all() ->
    [
     can_handle_successful_request,
     return_error_on_request_error
    ].

init_per_suite(Config) ->
    meck:new(hackney, [no_link]),
    Config.

end_per_suite(Config) ->
    meck:unload(),
    Config.

init_per_testcase(_Name, Config) ->
    meck:reset(hackney),

    meck:expect(hackney, request, [{['_', '_', '_', '_', '_'], {ok, ?A_CODE, [], ?A_BODY}}]),
    Config.

end_per_testcase(_Name, Config) ->
    Config.

can_handle_successful_request() ->
    [{doc, "When making a successful request, then returns body and code."}].
can_handle_successful_request(_Config) ->
    ?assertMatch({ok, {?A_CODE, ?A_BODY}}, treewalker_fetcher:request(?AN_URL, ?A_USER_AGENT, [])).

return_error_on_request_error() ->
    [{doc, "When making a failed request, then returns an error."}].
return_error_on_request_error(_Config) ->
    meck:expect(hackney, request, [{['_', '_', '_', '_', '_'], {error, nxdomain}}]),

    ?assertMatch({error, _}, treewalker_fetcher:request(?AN_URL, ?A_USER_AGENT, [])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
