%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @copyright 2019 Antoine Gagné
%% @doc
%% Filter URL and return whether or not they must be followed.
%% @end
-module(treewalker_link_filter).

%% API
-export([filter/1]).

-type url() :: treewalker_page:url().

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback filter(Url :: url()) -> {ok, IsAllowed :: boolean()} | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec filter(url()) -> {ok, true}.
filter(_Url) ->
    {ok, true}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
