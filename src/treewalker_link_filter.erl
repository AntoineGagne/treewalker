%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @copyright 2019 Antoine Gagné
%% @doc
%% Filter URL and return whether or not they must be followed.
%% @end
-module(treewalker_link_filter).

%% API
-export([]).

-type url() :: treewalker_page:url().

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback filter(Url :: url()) -> {ok, IsAllowed :: boolean()} | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% Internal functions
%%%===================================================================
