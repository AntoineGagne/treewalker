%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @copyright 2019 Antoine Gagné
%% @doc
%% Manipulate a scrapped page.
%% @end
-module(treewalker_page).

%% API
-export([init/0,
         url/1,
         url/2,
         content/1,
         content/2,
         name/1,
         name/2]).

-type name() :: binary().
-type url() :: uri_string:uri_string().
-type content() :: term().
-opaque page() :: #{url := url(),
                    content := content(),
                    name := name()}.

-export_type([name/0,
              url/0,
              page/0,
              content/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> page().
%% @doc Initialize a new scrapped page.
init() ->
    #{url => <<>>,
      content => #{},
      name => <<>>}.

-spec url(page()) -> url().
%% @doc Fetch the URL of the page.
url(#{url := Url}) ->
    Url.

-spec url(url(), page()) -> page().
%% @doc Set the URL of the page.
url(Url, Page) ->
    Page#{url := Url}.

-spec content(page()) -> content().
%% @doc Fetch the scrapped content of the page.
content(#{content := Content}) ->
    Content.

-spec content(content(), page()) -> page().
%% @doc Set the scrapped content of the page.
content(Content, Page) ->
    Page#{content := Content}.

-spec name(page()) -> name().
%% @doc Fetch the user agent that scrapped the page.
name(#{name := Name}) ->
    Name.

-spec name(name(), page()) -> page().
%% @doc Set the user agent that scrapped the page.
name(Name, Page) ->
    Page#{name := Name}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
