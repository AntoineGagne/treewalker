-module(treewalker_store).

%% API
-export([store/2]).

-type content() :: treewalker_scraper:content().
-type options() :: term().

-export_type([content/0,
              options/0]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback store(ScrapedContent :: content(), Options :: options()) -> ok.

%%%===================================================================
%%% API
%%%===================================================================

-spec store(ScrapedContent :: content(), Options :: options()) -> ok.
store(_Content, _Options) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
