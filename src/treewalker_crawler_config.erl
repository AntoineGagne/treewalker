-module(treewalker_crawler_config).

%% API
-export([init/0,
         merge/2,
         user_agent/1,
         user_agent/2,
         max_depth/1,
         max_depth/2,
         link_filter/1,
         link_filter/2,
         url/1,
         url/2,
         scraper/1,
         scraper/2,
         scraper_options/1,
         scraper_options/2,
         fetcher/1,
         fetcher/2,
         fetcher_options/1,
         fetcher_options/2]).

-type depth() :: non_neg_integer().
-type scraper() :: module().
-type scraper_options() :: treewalker_scraper:options().
-type fetcher() :: module().
-type fetcher_options() :: treewalker_fetcher:options().
-type link_filter() :: module().
-type url() :: treewalker_page:url().
-type user_agent() :: binary().

-opaque config() :: #{scraper := scraper(),
                      scraper_options := scraper_options(),
                      fetcher := fetcher(),
                      fetcher_options := fetcher_options(),
                      url := url(),
                      user_agent := binary(),
                      link_filter := link_filter(),
                      max_depth := depth()}.

-export_type([config/0,
              depth/0,
              scraper/0,
              scraper_options/0,
              fetcher/0,
              fetcher_options/0,
              link_filter/0,
              user_agent/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> config().
init() ->
    #{link_filter => treewalker_link_filter,
      url => <<>>,
      scraper => treewalker_scraper,
      scraper_options => [],
      fetcher => treewalker_fetcher,
      fetcher_options => [],
      user_agent => <<>>,
      max_depth => 1}.

-spec merge(config(), map()) -> config().
merge(Defaults, Custom) ->
    ValidKeys = valid_keys(),
    WithoutUnknown = maps:with(ValidKeys, Custom),
    maps:merge(Defaults, WithoutUnknown).

-spec url(config()) -> url().
url(#{url := Url}) ->
    Url.

-spec url(url(), config()) -> config().
url(Url, Config) ->
    Config#{url := Url}.

-spec user_agent(config()) -> binary().
user_agent(#{user_agent := UserAgent}) ->
    UserAgent.

-spec user_agent(binary(), config()) -> config().
user_agent(UserAgent, Config) ->
    Config#{user_agent := UserAgent}.

-spec max_depth(config()) -> depth().
max_depth(#{max_depth := MaxDepth}) ->
    MaxDepth.

-spec max_depth(depth(), config()) -> config().
max_depth(MaxDepth, Config) ->
    Config#{max_depth := MaxDepth}.

-spec link_filter(config()) -> link_filter().
link_filter(#{link_filter := LinkFilter}) ->
    LinkFilter.

-spec link_filter(link_filter(), config()) -> config().
link_filter(LinkFilter, Config) ->
    Config#{link_filter := LinkFilter}.

-spec scraper(config()) -> scraper().
scraper(#{scraper := Scraper}) ->
    Scraper.

-spec scraper(scraper(), config()) -> config().
scraper(Scraper, Config) ->
    Config#{scraper := Scraper}.

-spec scraper_options(config()) -> scraper_options().
scraper_options(#{scraper_options := ScrappingOptions}) ->
    ScrappingOptions.

-spec scraper_options(scraper_options(), config()) -> config().
scraper_options(Options, Config) ->
    Config#{scraper_options := Options}.

-spec fetcher(config()) -> fetcher().
fetcher(#{fetcher := Fetcher}) ->
    Fetcher.

-spec fetcher(fetcher(), config()) -> config().
fetcher(Fetcher, Config) ->
    Config#{fetcher := Fetcher}.

-spec fetcher_options(config()) -> fetcher_options().
fetcher_options(#{fetcher_options := ScrappingOptions}) ->
    ScrappingOptions.

-spec fetcher_options(fetcher_options(), config()) -> config().
fetcher_options(Options, Config) ->
    Config#{fetcher_options := Options}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

valid_keys() ->
    [scraper, scraper_options, fetcher, fetcher_options, link_filter, max_depth, url].
