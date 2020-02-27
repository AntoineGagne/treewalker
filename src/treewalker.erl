%%------------------------------------------------------------------------------
%% @doc This OTP application is used for crawling websites while respecting `robots.txt'.
%%
%% This module exposes some high level functions to be able to add new crawlers and start/stop them.
%%
%% While most of the configuration is per crawler, this application is also configurable globally
%% via the following `sys.config' settings:
%%
%%  ```
%%  {treewalker, [
%%                %% The minimum delay to wait before retrying a failed request
%%                {min_retry_delay, pos_integer()},
%%                %% The maximum delay to wait before retrying a failed request
%%                {max_retry_delay, pos_integer()},
%%                %% The maximum amount of retries of a failed request
%%                {max_retries, pos_integer()},
%%                %% The maximum amount of delay before starting a request (in seconds)
%%                {max_worker_delay, pos_integer()},
%%                %% The maximum amount of concurrent workers making HTTP requests
%%                {max_concurrent_worker, pos_integer()},
%%                %% The user agent making the HTTP requests
%%                {user_agent, binary()}]},
%%  '''
%%
%% @copyright 2020 Antoine Gagné
%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @end
%%------------------------------------------------------------------------------
-module(treewalker).

%% API
-export([add_crawler/2,
         add_crawler/3,
         remove_crawler/1,
         start_crawler/1,
         stop_crawler/1]).

-type child() :: treewalker_crawlers_sup:child().
-type options() :: #{scraper => module(),
                     scraper_options => term(),
                     fetcher => module(),
                     fetcher_options => module(),
                     max_depth => pos_integer(),
                     store => module(),
                     store_options => term(),
                     link_filter => module()}.
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
%% Remove the specified crawler.
%% @end
%%------------------------------------------------------------------------------
-spec remove_crawler(term()) -> ok.
remove_crawler(Name) ->
    treewalker_crawlers_sup:remove_crawler(Name).

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
%%
%% - `store': Module implementing the {@link treewalker_store} behaviour.
%%
%% - `store_options': The options to pass to the module implementing the
%%                    {@link treewalker_store} behaviour.
%%
%% - `link_filter': Module implementing the {@link treewalker_link_filter} behaviour.
%% @end
%%------------------------------------------------------------------------------
-spec add_crawler(term(), url(), options()) ->
    {ok, child()} | {ok, child(), term()} | {error, term()}.
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
