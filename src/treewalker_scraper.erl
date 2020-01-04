%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @copyright 2019 Antoine Gagné
%% @doc
%% Scrap the page for useful data.
%% @end
-module(treewalker_scraper).

%% API
-export([]).

-type url() :: treewalker_page:url().
-type page_data() :: binary().
-type content() :: treewalker_page:content().
-type options() :: term().

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback scrap(Url :: url(), PageData :: page_data(), ScrappingOptions :: options()) ->
    {ok, Scrapped :: content()} | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================
