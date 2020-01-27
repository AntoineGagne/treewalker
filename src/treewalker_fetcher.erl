-module(treewalker_fetcher).

-include_lib("kernel/include/logger.hrl").

%% API
-export([request/3]).

-export_type([options/0]).

-type options() :: term().
-type url() :: treewalker_page:url().
-type user_agent() :: treewalker_crawler_config:user_agent().

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback request(Url :: url(), UserAgent :: user_agent(), RequestOptions :: options()) ->
    {ok, Content :: binary()} | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec request(url(), user_agent(), options()) -> {ok, binary()}.
request(Url, UserAgent, Options) ->
    {ok, Ref, _} = hackney:request(get, Url, [{"User-Agent", UserAgent}], Options),
    {ok, Status, Headers, Ref} = hackney:start_response(Ref),
    ?LOG_DEBUG(#{what => request, url => Url, user_agent => UserAgent, request_ref => Ref,
                 response_headers => Headers, response_status => Status}),
    hackney:body(Ref).

%%%===================================================================
%%% Internal functions
%%%===================================================================
