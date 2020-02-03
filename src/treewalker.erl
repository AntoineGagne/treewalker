%%------------------------------------------------------------------------------
%% @doc Add and controls web crawlers.
%% @copyright 2020 Antoine Gagné
%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @end
%%------------------------------------------------------------------------------
-module(treewalker).

%% API
-export([add_crawler/2,
         add_crawler/3,
         start_crawler/1,
         stop_crawler/1]).

-type child() :: treewalker_crawlers_sup:child().
-type url() :: treewalker_page:url().

%%%===================================================================
%%% API
%%%===================================================================

%%------------------------------------------------------------------------------
%% @doc
%% Add a new crawler with the default configuration.
%% @end
%%------------------------------------------------------------------------------
-spec add_crawler(term(), url()) -> {ok, child()} | {ok, child(), term()} | {error, term()}.
add_crawler(Name, Url) ->
    treewalker_crawlers_sup:add_crawler(Name, #{url => Url}).

%%------------------------------------------------------------------------------
%% @doc
%% Add a new crawler with the specified configuration.
%%
%% The available options are as follow:
%%
%% - `scraper': Module implementing the {@link treewalker_scraper} behaviour.
%%
%% - `scraper_options': The options to pass to the module implementing the
%%                      {@link treewalker_scraper} behaviour.
%%
%% - `fetcher': Module implementing the {@link treewalker_fetcher} behaviour.
%%
%% - `fetcher_options': The options to pass to the module implementing the
%%                      {@link treewalker_fetcher} behaviour.
%%
%% - `max_depth': The max depth that the crawler will crawl.
%% @end
%%------------------------------------------------------------------------------
-spec add_crawler(term(), url(), map()) -> {ok, child()} | {ok, child(), term()} | {error, term()}.
add_crawler(Name, Url, Custom) ->
    treewalker_crawlers_sup:add_crawler(Name, Custom#{url => Url}).

%%------------------------------------------------------------------------------
%% @doc
%% Start the specified crawler.
%% @end
%%------------------------------------------------------------------------------
-spec start_crawler(term()) -> ok.
start_crawler(Name) ->
    treewalker_crawlers_sup:start_crawler(Name).

%%------------------------------------------------------------------------------
%% @doc
%% Stop the specified crawler.
%% @end
%%------------------------------------------------------------------------------
-spec stop_crawler(term()) -> ok.
stop_crawler(Name) ->
    treewalker_crawlers_sup:stop_crawler(Name).

%%%===================================================================
%%% Internal functions
%%%===================================================================
