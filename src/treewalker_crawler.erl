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
               dispatcher_id :: dispatcher_id()}).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).

-type config() :: treewalker_crawler_config:config().
-type id() :: treewalker_crawler_sup:crawler_id().
-type dispatcher_id() :: treewalker_crawler_sup:dispatcher_id().

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

handle_event(internal, _Event, _State, _Data) ->
    keep_state_and_data;

handle_event(cast, start, stopped, Data=#data{config = Config}) ->
    Url = treewalker_crawler_config:url(Config),
    ?LOG_INFO(#{what => crawl, status => start, start => Url, depth => 1}),
    UpdatedData = crawl(Url, Data),
    {next_state, started, UpdatedData};
handle_event(cast, start, started, _Data) ->
    ?LOG_WARNING(#{what => crawler_start, status => done, result => error,
                   reason => already_started}),
    keep_state_and_data;

handle_event(cast, _Message, _State, _Data) ->
    keep_state_and_data;

handle_event({call, From}, _Action, State, Data) ->
    {next_state, State, Data, [{reply, From, Data}]};

handle_event(info, {treewalker_dispatcher, Ref, Url, {ok, Result}}, started,
             Data=#data{config = Config}) ->
    ?LOG_DEBUG(#{what => crawl, event => message_received, status => in_progress,
                 id => Ref, url => Url}),
    Scraper = treewalker_crawler_config:scraper(Config),
    Options = treewalker_crawler_config:scraper_options(Config),
    LinkFilter = treewalker_crawler_config:link_filter(Config),
    UnfilteredUrls = Scraper:scrap_links(Url, Result, Options),
    FilteredUrls = lists:filter(fun LinkFilter:filter/1, UnfilteredUrls),
    UpdatedData = lists:foldl(fun crawl/2, Data, FilteredUrls),
    {keep_state, UpdatedData};
handle_event(info, {treewalker_dispatcher, Ref, Url, Error={error, _}}, started, _Data) ->
    ?LOG_WARNING(#{what => crawl, event => message_received, status => in_progress, result => error,
                   url => Url, id => Ref, reason => Error}),
    keep_state_and_data;
handle_event(info, {treewalker_dispatcher, Ref, Url, _Result}, stopped, _Data) ->
    ?LOG_WARNING(#{what => response_received, id => Ref, url => Url, status => done,
                   result => ignored, reason => crawler_stopped}),
    keep_state_and_data;

handle_event(info, Message, State, _Data) ->
    ?LOG_WARNING(#{what => unexpected_message, message => Message, state => State}),
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

robots(#data{config = Config}) ->
    Url = treewalker_crawler_config:url(Config),
    Fetcher = treewalker_crawler_config:fetcher(Config),
    Options = treewalker_crawler_config:fetcher_options(Config),
    UserAgent = treewalker_crawler_config:user_agent(Config),
    {ok, Content} = Fetcher:request(Url, UserAgent, Options),
    robots:parse(Content, 200).

crawl(Url, Data) ->
    _Ref = treewalker_dispatcher:request(Data#data.dispatcher_id, Url),
    Data.
