-module(treewalker_crawler).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_statem).

%% API
-export([start_link/3,
         start_crawler/1,
         stop_crawler/1]).

%% gen_statem callbacks
-export([init/1,
         callback_mode/0,
         handle_event/4,
         terminate/3]).

-record(data, {config :: config(),
               retry_timeout :: pos_integer(),
               visited_pages = sets:new() :: sets:set(url()),
               requests_by_ids = #{} :: requests_by_ids(),
               dispatcher_id :: dispatcher_id()}).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).

-type config() :: treewalker_crawler_config:config().
-type requests_by_ids() :: #{request_id() := {url(), depth()}}.
-type depth() :: treewalker_crawler_config:depth().
-type id() :: treewalker_crawler_sup:crawler_id().
-type url() :: treewalker_page:url().
-type request_id() :: reference().
-type dispatcher_id() :: treewalker_crawler_sup:dispatcher_id().
-type agent_rules() :: robots:agent_rules().
-type either(Left, Right) :: {ok, Left} | {error, Right}.

-type data() :: #data{}.

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

-spec stop_crawler(id()) -> ok.
stop_crawler(Id) ->
    gen_statem:cast(?VIA_GPROC(Id), stop).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() ->
    [handle_event_function].

init([DispatcherId, Config]) ->
    RetryTimeout = application:get_env(treewalker, retry_interval, ?RETRY_TIMEOUT),
    {ok, stopped, #data{config = Config,
                        dispatcher_id = DispatcherId,
                        retry_timeout = RetryTimeout}}.

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

handle_event(cast, stop, _State, Data) ->
    ?LOG_INFO(#{what => crawler_start, status => start}),
    NewData = Data#data{requests_by_ids = #{}, visited_pages = sets:new()},
    {next_state, stopped, NewData};

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

-spec walk(binary(), request_id(), agent_rules(), data()) -> data().
walk(Content, Ref, Robots, Data=#data{config = Config}) ->
    MaxDepth = treewalker_crawler_config:max_depth(Config),
    case maps:take(Ref, Data#data.requests_by_ids) of
        {{Url, Depth}, RequestsByIds} when Depth =< MaxDepth ->
            ok = try_store(Url, Content, Config),
            VisitedPages = sets:add_element(Url, Data#data.visited_pages),
            UpdatedData = Data#data{requests_by_ids = RequestsByIds, visited_pages = VisitedPages},
            Scraper = treewalker_crawler_config:scraper(Config),
            Options = treewalker_crawler_config:scraper_options(Config),
            Result = Scraper:scrap_links(Url, Content, Options),
            maybe_walk(Result, Depth, Robots, UpdatedData);
        {{Url, Depth}, RequestsByIds} ->
            ?LOG_INFO(#{what => walk, status => done, reason => max_depth_reached, url => Url,
                        max_depth => MaxDepth, depth => Depth}),
            VisitedPages = sets:add_element(Url, Data#data.visited_pages),
            Data#data{requests_by_ids = RequestsByIds, visited_pages = VisitedPages};
        error ->
            Data
    end.

-spec try_store(url(), treewalker_scraper:page_data(), config()) -> ok.
try_store(Url, Content, Config) ->
    Scraper = treewalker_crawler_config:scraper(Config),
    Options = treewalker_crawler_config:scraper_options(Config),
    ?LOG_DEBUG(#{what => store, status => start, url => Url}),
    case Scraper:scrap(Url, Content, Options) of
        {ok, Scraped} ->
            ?LOG_DEBUG(#{what => store, url => Url, status => done, result => ok}),
            Page = page(Url, Scraped, Config),
            Store = treewalker_crawler_config:store(Config),
            StoreOptions = treewalker_crawler_config:store_options(Config),
            Store:store(Page, StoreOptions);
        Error={error, _} ->
            ?LOG_ERROR(#{what => store, url => Url, status => done, result => error,
                         reason => Error}),
            ok
    end.

-spec maybe_walk(either([url()], term()), depth(), agent_rules(), data()) -> data().
maybe_walk({ok, Links}, Depth, Robots, Data) ->
    Filter = filter_link(Data, Robots),
    Filtered = lists:filtermap(Filter, Links),
    lists:foldl(fun (Url, Acc) -> crawl(Url, Depth + 1, Acc) end, Data, Filtered);
maybe_walk(Error={error, _}, _Depth, _Robots, Data) ->
    ?LOG_WARNING(#{what => walk, status => done, result => error, reason => Error}),
    Data.

-spec crawl(url(), depth(), data()) -> data().
crawl(Url, Depth, Data=#data{requests_by_ids = RequestsByIds}) ->
    Ref = treewalker_dispatcher:request(Data#data.dispatcher_id, Url),
    Data#data{requests_by_ids = RequestsByIds#{Ref => {Url, Depth}}}.

-spec filter_link(data(), agent_rules()) -> fun ((url()) -> {true, url()} | false).
filter_link(Data, Robots) ->
    Filter = filter(Data, Robots),
    Normalize = normalize_relative_url(Data),
    fun (Url) ->
            case Normalize(Url) of
                {ok, Normalized} ->
                    case Filter(Normalized) of
                        true ->
                            {true, Normalized};
                        false ->
                            false
                    end;
                Error={error, _} ->
                    ?LOG_WARNING(#{what => walk, status => in_progress,
                                   result => skipping, url => Url, reason => Error}),
                    false
            end
    end.

-spec normalize_relative_url(data()) -> fun ((url()) -> either(url(), term())).
normalize_relative_url(#data{config = Config}) ->
    Url = treewalker_crawler_config:url(Config),
    UriMap = uri_string:parse(Url),
    Updated = UriMap#{path => <<>>},
    fun (Other) ->
            case uri_string:parse(Other) of
                {error, E, I} ->
                    {error, {E, I}};
                OtherUriMap ->
                    Merged = maps:merge(Updated, OtherUriMap),
                    {ok, uri_string:recompose(Merged)}
            end
    end.

-spec filter(data(), agent_rules()) -> fun ((url()) -> boolean()).
filter(#data{config = Config, visited_pages = VisitedPages}, Robots) ->
    LinkFilter = treewalker_crawler_config:link_filter(Config),
    UserAgent = treewalker_crawler_config:user_agent(Config),
    fun (Url) ->
            Filtered = LinkFilter:filter(Url),
            #{path := Path} = uri_string:parse(Url),
            Allowed = robots:is_allowed(UserAgent, Path, Robots),
            Visited = sets:is_element(Url, VisitedPages),
            Filtered andalso Allowed andalso not Visited
    end.

try_fetch_robots(Url, Data=#data{config = Config}) ->
    Fetcher = treewalker_crawler_config:fetcher(Config),
    Options = treewalker_crawler_config:fetcher_options(Config),
    UserAgent = treewalker_crawler_config:user_agent(Config),
    case Fetcher:request(Url, UserAgent, Options) of
        {ok, {Code, Body}} ->
            try_parse_robots(Code, Body, Data);
        Error={error, _} ->
            ?LOG_ERROR(#{what => robots_fetch, status => done, result => error, reason => Error}),
            {keep_state_and_data, {{timeout, retry}, Data#data.retry_timeout, retry}}
    end.

try_parse_robots(Code, Body, Data) ->
    case robots:parse(Body, Code) of
        {ok, Robots} ->
            {next_state, {crawling, Robots}, Data, {next_event, internal, crawl}};
        Error={error, _} ->
            ?LOG_ERROR(#{what => robots_fetch, status => done, result => error, reason => Error}),
            {keep_state_and_data, {{timeout, retry}, Data#data.retry_timeout, retry}}
    end.

-spec page(url(), treewalker_page:content(), config()) -> treewalker_page:page().
page(Url, ScrapedContent, Config) ->
    Page = treewalker_page:init(),
    UserAgent = treewalker_crawler_config:user_agent(Config),
    WithUrl = treewalker_page:url(Url, Page),
    WithContent = treewalker_page:content(ScrapedContent, WithUrl),
    treewalker_page:name(UserAgent, WithContent).
