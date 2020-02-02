-module(treewalker_fetcher).

-include_lib("kernel/include/logger.hrl").

%% API
-export([request/3]).

-export_type([options/0]).

-type options() :: term().
-type url() :: treewalker_page:url().
-type user_agent() :: treewalker_crawler_config:user_agent().
-type status_code() :: 100..599.

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback request(Url :: url(), UserAgent :: user_agent(), RequestOptions :: options()) ->
    {ok, {Code :: status_code(), Content :: binary()}} | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec request(url(), user_agent(), options()) -> {ok, {status_code(), binary()}} | {error, term()}.
request(Url, UserAgent, Options) ->
    ?LOG_DEBUG(#{what => request, url => Url, user_agent => UserAgent, status => start}),
    Headers = [{<<"User-Agent">>, UserAgent}],
    case hackney:request(get, Url, Headers, Options, []) of
        {ok, Code, _, Ref} ->
            ?LOG_DEBUG(#{what => request, url => Url, user_agent => UserAgent, status => done,
                         status_code => Code}),
            case hackney:body(Ref) of
                {ok, Body} ->
                    {ok, {Code, Body}};
                Error={error, _} ->
                    ?LOG_DEBUG(#{what => request, url => Url, user_agent => UserAgent,
                                 status => done, result => error, reason => Error}),
                    Error
            end;
        Error={error, _} ->
            ?LOG_DEBUG(#{what => request, url => Url, user_agent => UserAgent, status => done,
                         result => error, reason => Error}),
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
