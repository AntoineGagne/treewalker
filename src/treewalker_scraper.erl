%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @copyright 2019 Antoine Gagné
%% @doc
%% Scrap the page for useful data and links.
%% @end
-module(treewalker_scraper).

%% API
-export([scrap/3,
         scrap_links/3]).

-export_type([options/0]).

-optional_callbacks([scrap_links/3]).

-type url() :: treewalker_page:url().
-type page_data() :: binary().
-type content() :: treewalker_page:content().
-type options() :: term().

%%%===================================================================
%%% Callbacks
%%%===================================================================

-callback scrap(Url :: url(), PageData :: page_data(), ScrappingOptions :: options()) ->
    {ok, Scrapped :: content()} | {error, Reason :: term()}.

-callback scrap_links(Url :: url(), PageData :: page_data(), ScrappingOptions :: options()) ->
    {ok, Links :: [url()]} | {error, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec scrap(url(), page_data(), options()) -> {ok, content()}.
scrap(_Url, _PageData, _ScrappingOptions) ->
    {ok, #{}}.

-spec scrap_links(url(), page_data(), options()) -> {ok, [url()]} | {error, term()}.
scrap_links(_Url, PageData, _ScrappingOptions) ->
    case ecureuil:find(<<"a">>, PageData) of
        {ok, Nodes} ->
            {ok, lists:filtermap(fun extract_link/1, Nodes)};
        Error={error, _} ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

extract_link(Node) ->
    case ecureuil_html:attribute(Node, <<"href">>) of
        {ok, Link} -> {true, Link};
        {error, _} -> false
    end.
