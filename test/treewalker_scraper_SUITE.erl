-module(treewalker_scraper_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(AN_URL, <<"http://anurl.com/some/path">>).
-define(SOME_OPTIONS, #{}).
-define(SOME_INVALID_HTML, <<"invalid">>).
-define(SOME_VALID_HTML_WITH_NO_LINKS, <<"<html><body><div>no links</div></body></html>">>).
-define(A_LINK, <<"http://alink.com/some/path">>).
-define(SOME_HTML_WITH_LINK,
        <<"<html><body><a href=\"", ?A_LINK/binary, "\">a link</a></body></html>">>).

all() ->
    [
     return_error_on_invalid_html_page_while_scrapping_links,
     return_empty_list_on_page_with_no_links,
     return_links_on_page_with_links
    ].

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    Config.

return_error_on_invalid_html_page_while_scrapping_links() ->
    [{doc, "Given a page with invalid HTML, when scrapping links, then returns an error."}].
return_error_on_invalid_html_page_while_scrapping_links(_Config) ->
    ?assertMatch({error, _},
                 treewalker_scraper:scrap_links(?AN_URL, ?SOME_INVALID_HTML, ?SOME_OPTIONS)).

return_empty_list_on_page_with_no_links() ->
    [{doc, "Given a page with no links, when scrapping links, then returns an empty list."}].
return_empty_list_on_page_with_no_links(_Config) ->
    ?assertMatch({ok, []},
                 treewalker_scraper:scrap_links(?AN_URL,
                                                ?SOME_VALID_HTML_WITH_NO_LINKS,
                                                ?SOME_OPTIONS)).

return_links_on_page_with_links() ->
    [{doc, "Given a page with links, when scrapping links, then returns all links."}].
return_links_on_page_with_links(_Config) ->
    ?assertMatch({ok, [?A_LINK]},
                 treewalker_scraper:scrap_links(?AN_URL, ?SOME_HTML_WITH_LINK, ?SOME_OPTIONS)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
