-module(treewalker_crawler_config).

%% API
-export([init/0,
         max_depth/1,
         max_depth/2,
         link_filter/1,
         link_filter/2,
         scraper/1,
         scraper/2]).

-type depth() :: non_neg_integer().
-type scraper() :: module().
-type scraper_options() :: treewalker_scraper:options().
-type link_filter() :: module().

-opaque config() :: #{scraper := scraper(),
                      scraper_options := scraper_options(),
                      link_filter := link_filter(),
                      max_depth := depth()}.

-export_type([config/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> config().
init() ->
    #{link_filter => treewalker_link_filter,
      scraper => treewalker_scraper,
      scraper_options => [],
      max_depth => 1}.

max_depth(#{max_depth := MaxDepth}) ->
    MaxDepth.

max_depth(MaxDepth, Config) ->
    Config#{max_depth := MaxDepth}.

link_filter(#{link_filter := LinkFilter}) ->
    LinkFilter.

link_filter(LinkFilter, Config) ->
    Config#{link_filter := LinkFilter}.

scraper(#{scraper := Scraper}) ->
    Scraper.

scraper(Scraper, Config) ->
    Config#{scraper := Scraper}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
