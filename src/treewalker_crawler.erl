-module(treewalker_crawler).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/3,
         start_crawler/1]).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         handle_event/4,
         terminate/3]).

-record(data, {config :: config(),
               requests_by_ids = #{} :: requests_by_ids(),
               dispatcher_id :: dispatcher_id()}).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).

-type config() :: treewalker_crawler_config:config().
-type requests_by_ids() :: #{reference() := {url(), depth()}}.
-type depth() :: treewalker_crawler_config:depth().
-type id() :: treewalker_crawler_sup:crawler_id().
-type url() :: treewalker_page:url().
-type dispatcher_id() :: treewalker_crawler_sup:dispatcher_id().

-define(RETRY_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(id(), dispatcher_id(), config()) -> {ok, pid()} | {error, term()}.
start_link(Id, DispatcherId, Config) ->
    gen_statem:start_link(?VIA_GPROC(Id), ?MODULE, [DispatcherId, Config], []).

-spec start_crawler(id()) -> ok.
start_crawler(Id) ->
    gen_statem:cast(?VIA_GPROC(Id), start).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    [handle_event_function].

init([DispatcherId, Config]) ->
    {ok, stopped, #data{config = Config, dispatcher_id = DispatcherId}}.

handle_event(internal, fetch_robots, started, Data=#data{config = Config}) ->
    Url = treewalker_crawler_config:url(Config),
    ?LOG_DEBUG(#{what => robots_fetch, status => start, url => Url}),
    UriMap = uri_string:parse(Url),
    Recomposed = uri_string:recompose(UriMap#{path => <<"/robots.txt">>}),
    try_fetch_robots(Recomposed, Data);

handle_event(internal, crawl, {crawling, _Robots}, Data=#data{config = Config}) ->
    Url = treewalker_crawler_config:url(Config),
    UpdatedData = crawl(Url, 0, Data),
    {keep_state, UpdatedData};

handle_event(cast, start, _State, Data) ->
    ?LOG_INFO(#{what => crawler_start, status => start}),
    NewData = Data#data{requests_by_ids = #{}},
    {next_state, started, NewData, {next_event, internal, fetch_robots}};

handle_event(cast, _Message, _State, _Data) ->
    keep_state_and_data;

handle_event({call, From}, _Action, State, Data) ->
    {next_state, State, Data, [{reply, From, Data}]};

handle_event(info, {treewalker_dispatcher, Ref, {ok, {Code, Result}}}, {crawling, Robots}, Data) ->
    ?LOG_DEBUG(#{what => crawl, event => message_received, status => in_progress, code => Code,
                 id => Ref}),
    UpdatedData = walk(Result, Ref, Robots, Data),
    {keep_state, UpdatedData};
handle_event(info, {treewalker_dispatcher, Ref, Error={error, _}}, {crawling, _Robots},
             _Data) ->
    ?LOG_WARNING(#{what => crawl, event => message_received, status => in_progress,
                   result => error, id => Ref, reason => Error}),
    keep_state_and_data;
handle_event(info, {treewalker_dispatcher, Ref, Url, _Result}, stopped, _Data) ->
    ?LOG_WARNING(#{what => response_received, id => Ref, url => Url, status => done,
                   result => ignored, reason => crawler_stopped}),
    keep_state_and_data;

handle_event(info, Message, State, _Data) ->
    ?LOG_WARNING(#{what => unexpected_message, message => Message, state => State}),
    keep_state_and_data;

handle_event({timeout, retry}, retry, started, _Data) ->
    {keep_state_and_data, {next_event, internal, fetch_robots}};

handle_event(Event, Message, State, _Data) ->
    ?LOG_WARNING(#{what => unexpected_event, event => Event, message => Message, state => State}),
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

walk(Content, Ref, Robots, Data=#data{config = Config}) ->
    MaxDepth = treewalker_crawler_config:max_depth(Config),
    case maps:take(Ref, Data#data.requests_by_ids) of
        {{Url, Depth}, RequestsByIds} when Depth =< MaxDepth ->
            Scraper = treewalker_crawler_config:scraper(Config),
            Options = treewalker_crawler_config:scraper_options(Config),
            Result = Scraper:scrap_links(Url, Content, Options),
            maybe_walk(Result, Depth, Robots, Data#data{requests_by_ids = RequestsByIds});
        {{Url, Depth}, RequestsByIds} ->
            ?LOG_INFO(#{what => walk, status => done, reason => max_depth_reached, url => Url,
                        max_depth => MaxDepth, depth => Depth}),
            Data#data{requests_by_ids = RequestsByIds};
        error ->
            Data
    end.

maybe_walk({ok, Links}, Depth, Robots, Data=#data{config = Config}) ->
    Filter = filter(Data#data.config, Robots),
    Filtered = lists:filter(Filter, Links),
    Normalize = normalize_relative_url(Config),
    lists:foldl(fun (Url, Acc) -> crawl(Normalize(Url), Depth + 1, Acc) end, Data, Filtered);
maybe_walk(Error={error, _}, _Depth, _Robots, Data) ->
    ?LOG_WARNING(#{what => walk, status => done, result => error, reason => Error}),
    Data.

normalize_relative_url(Config) ->
    Url = treewalker_crawler_config:url(Config),
    UriMap = uri_string:parse(Url),
    Updated = UriMap#{path => <<>>},
    fun (Other) ->
            OtherUriMap = uri_string:parse(Other),
            Merged = maps:merge(Updated, OtherUriMap),
            uri_string:recompose(Merged)
    end.

crawl(Url, Depth, Data=#data{requests_by_ids = RequestsByIds}) ->
    Ref = treewalker_dispatcher:request(Data#data.dispatcher_id, Url),
    Data#data{requests_by_ids = RequestsByIds#{Ref => {Url, Depth}}}.

filter(Config, Robots) ->
    LinkFilter = treewalker_crawler_config:link_filter(Config),
    UserAgent = treewalker_crawler_config:user_agent(Config),
    fun (Url) -> LinkFilter:filter(Url) andalso robots:is_allowed(UserAgent, Url, Robots) end.

try_fetch_robots(Url, Data=#data{config = Config}) ->
    Fetcher = treewalker_crawler_config:fetcher(Config),
    Options = treewalker_crawler_config:fetcher_options(Config),
    UserAgent = treewalker_crawler_config:user_agent(Config),
    case Fetcher:request(Url, UserAgent, Options) of
        {ok, {Code, Body}} ->
            try_parse_robots(Code, Body, Data);
        Error={error, _} ->
            ?LOG_ERROR(#{what => robots_fetch, status => done, result => error, reason => Error}),
            {keep_state_and_data, {{timeout, retry}, ?RETRY_TIMEOUT, retry}}
    end.

try_parse_robots(Code, Body, Data) ->
    case robots:parse(Body, Code) of
        {ok, Robots} ->
            {next_state, {crawling, Robots}, Data, {next_event, internal, crawl}};
        Error={error, _} ->
            ?LOG_ERROR(#{what => robots_fetch, status => done, result => error, reason => Error}),
            {keep_state_and_data, {{timeout, retry}, ?RETRY_TIMEOUT, retry}}
    end.
