-module(treewalker_store).

%% API
-export([store/2]).

-type options() :: term().
-type page() :: treewalker_page:page().

-export_type([options/0]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback store(ScrapedPage :: page(), Options :: options()) -> ok.

%%%===================================================================
%%% API
%%%===================================================================

-spec store(ScrapedPage :: page(), Options :: options()) -> ok.
store(_Content, _Options) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
