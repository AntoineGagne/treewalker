%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @copyright 2019 Antoine Gagné
%% @doc
%% Store the scrapped page.
%% @end
-module(treewalker_store).

%% API
-export([]).

-type options() :: term().
-type page() :: treewalker_page:page().

-export_type([options/0]).

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback store(Page :: page(), StoreOptions :: options()) ->
    ok | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================
