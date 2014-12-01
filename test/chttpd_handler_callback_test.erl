%% Licensed under the Apache License,  Version 2.0 (the "License");  you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%                 http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless  required  by  applicable  law  or  agreed  to  in  writing,  software
%% distributed  under the  License  is distributed on an "AS IS" BASIS,  WITHOUT
%% WARRANTIES  OR  CONDITIONS  OF  ANY  KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.
%%
%% @doc
%% These whitebox tests call the endpoints that are implemented by dynamic funs,
%% which replace the previously hardwired handlers, with a dynamically created
%% module. This test mocks everything except the proper relationship between
%% handler function clause and returned fun. The handler functions are called
%% directly.
%%
%% This can look like mocks testing themselves. In this case, in other words:
%% think of the former url_handler function in chttpd.erl: The relationship
%% between a function clause and the specific returned fun() is exactly what is
%% established by the dynamically created module. And it's precisely what this
%% test verifies, for every endpoint.
%%
%% This test was part of chttpd-cloudant dbcore-2.7.7, verifying the original
%% hardcoded endpoints. It has since been reworked but remains functionally
%% mostly identical; the config source and processing has been replaced.

-module(chttpd_handler_callback_test).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    io:format(user, "~nEndpoint handler callbacks:~n", []),
    {setup,
    fun() -> chttpd_handler:build(test_cfg()) end,

    % url handlers
    [fun test_empty_string/0,
    fun test_favicon/0,
    fun test_utils/0,
    fun test_all_dbs/0,
    fun test_active_tasks/0,
    fun test_config/0,
    fun test_reload_query_servers/0,
    fun test_replicate/0,
    fun test_uuids/0,
    fun test_sleep/0,
    fun test_session/0,
    fun test_oauth/0,
    fun test_up/0,
    fun test_membership/0,
    fun test_db_updates/0,
    fun test_anything/0,

    % Test the tests: if the final target function is missing, the call must
    % fail, with any parameter. All parameters are valid.
    fun  verify_unmocked_failing_empty_string/0,
    fun verify_unmocked_failing_favicon/0,
    fun verify_unmocked_failing_anything/0,
 
    % db url handler tests,
    fun test_view_cleanup/0,
    fun test_compact/0,
    fun test_design/0,
    fun test_temp_view/0,
    fun test_changes/0,
    fun test_shards/0,
    
    % Test the test: when the final target function is missing, the Fun call
    % must fail, with any argument, including valid ones.
    fun  verify_unmocked_failing_view_cleanup/0,
    fun verify_unmocked_db_failing_something/0,

    % design url handler tests
    fun test_view/0,
    fun test_show/0,
    fun test_list/0,
    fun test_update/0,
    fun test_info/0,
    fun test_rewrite/0,

    % Test the test: when the final target function is missing, the Fun call
    % must fail with any argument, including valid ones.
    fun verify_unmocked_failing_view/0,
    fun verify_unmocked_design_failing_something/0]}.

test_empty_string() ->
    assertReturns("", chttpd_misc, handle_welcome_req).

test_favicon() ->
    assertReturns("favicon.ico", chttpd_misc, handle_favicon_req).

test_utils() ->
    assertReturns("_utils", chttpd_misc, handle_utils_dir_req).

test_all_dbs() ->
    assertReturns("_all_dbs", chttpd_misc, handle_all_dbs_req).

test_active_tasks() ->
    assertReturns("_active_tasks", chttpd_misc, handle_task_status_req).

test_config() ->
    assertReturns("_config", chttpd_misc, handle_config_req).

test_reload_query_servers() ->
    assertReturns("_reload_query_servers", chttpd_misc,
        handle_reload_query_servers_req).

test_replicate() ->
    assertReturns("_replicate", chttpd_misc, handle_replicate_req).

test_uuids() ->
    assertReturns("_uuids", chttpd_misc, handle_uuids_req).

test_sleep() ->
    assertReturns("_sleep", chttpd_misc, handle_sleep_req).

test_session() ->
    assertReturns("_session", chttpd_auth, handle_session_req).

test_oauth() ->
    assertReturns("_oauth", couch_httpd_oauth, handle_oauth_req).

test_up() ->
    assertReturns("_up", chttpd_misc, handle_up_req).

test_membership() ->
    assertReturns("_membership", mem3_httpd, handle_membership_req).

test_db_updates() ->
    assertReturns("_db_updates", global_changes_httpd,
        handle_global_changes_req).

test_anything() ->
    assertReturns("anything", chttpd_db, handle_request).

verify_unmocked_failing_empty_string() ->
    assertUnmockedFails("", chttpd_misc).

verify_unmocked_failing_favicon() ->
    assertUnmockedFails("favicon.ico", chttpd_misc).

verify_unmocked_failing_anything() ->
    assertUnmockedFails(anything, chttpd_db).

test_view_cleanup() ->
    assertReturns(db_url_handlers, <<"_view_cleanup">>, chttpd_db,
        handle_view_cleanup_req, 2).

test_compact() ->
    assertReturns(db_url_handlers, <<"_compact">>, chttpd_db,
        handle_compact_req, 2).

test_design() ->
    assertReturns(db_url_handlers, <<"_design">>, chttpd_db,
        handle_design_req, 2).

test_temp_view() ->
    assertReturns(db_url_handlers, <<"_temp_view">>, chttpd_view,
        handle_temp_view_req, 2).

test_changes() ->
    assertReturns(db_url_handlers, <<"_changes">>, chttpd_db,
        handle_changes_req, 2).

test_shards() ->
    assertReturns(db_url_handlers, <<"_shards">>, mem3_httpd,
        handle_shards_req, 2).

verify_unmocked_failing_view_cleanup() ->
    assertUnmockedFails(db_url_handlers, <<"_view_cleanup">>, chttpd_db, 2).

verify_unmocked_db_failing_something() ->
    assertUnknownFails(db_url_handlers, <<"_something">>).

test_view() ->
    assertReturns(design_url_handlers, <<"_view">>, chttpd_view,
        handle_view_req, 3).

test_show() ->
    assertReturns(design_url_handlers, <<"_show">>, chttpd_show,
        handle_doc_show_req, 3).

test_list() ->
    assertReturns(design_url_handlers, <<"_list">>, chttpd_show,
        handle_view_list_req, 3).

test_update() ->
    assertReturns(design_url_handlers, <<"_update">>, chttpd_show,
        handle_doc_update_req, 3).

test_info() ->
    assertReturns(design_url_handlers, <<"_info">>, chttpd_db,
        handle_design_info_req, 3).

test_rewrite() ->
    assertReturns(design_url_handlers, <<"_rewrite">>, chttpd_rewrite,
        handle_rewrite_req, 3).

verify_unmocked_failing_view() ->
    assertUnmockedFails(design_url_handlers, <<"_view">>, chttpd_view, 3).

verify_unmocked_design_failing_something() ->
    assertUnknownFails(design_url_handlers, <<"_something">>).


%% Call the dynamic function with a parameter known to trigger a specific
%% clause. Then call the returned fun and get confirmation that this called
%% the right implicit fun, which is mocked, and returns the expected tuple.
assertReturns(Endpoint, M, F) ->
    meck:new(M, [passthrough, non_strict]),
    try
        io:format(user, "~-47...s ", [Endpoint]),
        meck:expect(M, F, fun(X) -> {return, Endpoint, X} end),
        Fun = chttpd_handler:url_handler(Endpoint),
        ?_assertEqual({return, Endpoint, x}, Fun(x))
    after
        meck:unload(M)
    end.

%% Test that the indirection does NOT work when the function does not exist.
assertUnmockedFails(Endpoint, M) ->
    meck:new(M, [non_strict]),
    try
        Fun = chttpd_handler:url_handler(Endpoint),
        ?_assertError(undef, Fun(x))
    after
        meck:unload(M)
    end.

%% Same as assertReturns/3 but for dynamic functions that return a list of funs.
assertReturns(HandlerLister, Endpoint, M, F, Arity) ->
    meck:new(M, [passthrough, non_strict]),
    try
        io:format(user, "~-47...s ", [Endpoint]),
        case Arity of
        2 ->
            meck:expect(M, F, fun(X, Y) -> {return, Endpoint, X, Y} end),
            {_, Fun} = lists:keyfind(Endpoint, 1, chttpd_handler:HandlerLister()),
            ?_assertEqual({return, Endpoint, x, y}, Fun(x, y));
        3 ->
            meck:expect(M, F, fun(X, Y, Z) -> {return, Endpoint, X, Y, Z} end),
            {_, Fun} = lists:keyfind(Endpoint, 1, chttpd_handler:HandlerLister()),
            ?_assertEqual({return, Endpoint, x, y, z}, Fun(x, y, z))
        end
    after
        meck:unload(M)
    end.

%% Test that the indirection does NOT work when the function does not exist.
assertUnmockedFails(HandlerLister, Endpoint, M, Arity) ->
    meck:new(M, [non_strict]),
    try
        {_, Fun} = lists:keyfind(Endpoint, 1, chttpd_handler:HandlerLister()),
        case Arity of
        2 -> ?_assertError(undef, Fun(x, y));
        3 -> ?_assertError(undef, Fun(x, y, z))
        end
    after
        meck:unload(M)
    end.

%% Make sure that a wrong parameter also really fails.
assertUnknownFails(HandlerLister, Endpoint) ->
    false = lists:keyfind(Endpoint, 1, chttpd_handler:HandlerLister()).

test_cfg() ->
[{url_handler, clauses, [
    {"",                      {chttpd_misc, handle_welcome_req, 1}},
    {"favicon.ico",           {chttpd_misc, handle_favicon_req, 1}},
    {"_utils",                {chttpd_misc, handle_utils_dir_req, 1}},
    {"_all_dbs",              {chttpd_misc, handle_all_dbs_req, 1}},
    {"_active_tasks",         {chttpd_misc, handle_task_status_req, 1}},
    {"_config",               {chttpd_misc, handle_config_req, 1}},
    {"_reload_query_servers", {chttpd_misc,
        handle_reload_query_servers_req, 1}},
    {"_replicate",            {chttpd_misc, handle_replicate_req, 1}},
    {"_uuids",                {chttpd_misc, handle_uuids_req, 1}},
    {"_sleep",                {chttpd_misc, handle_sleep_req, 1}},
    {"_session",              {chttpd_auth, handle_session_req, 1}},
    {"_oauth",                {couch_httpd_oauth, handle_oauth_req, 1}},
    {"_up",                   {chttpd_misc, handle_up_req, 1}},
    {"_membership",           {mem3_httpd, handle_membership_req, 1}},
    {"_db_updates",           {global_changes_httpd,
        handle_global_changes_req, 1}},
    {'_',                     {chttpd_db, handle_request, 1}}]},
{db_url_handlers, list, [
    {<<"_view_cleanup">>,     {chttpd_db, handle_view_cleanup_req, 2}},
    {<<"_compact">>,          {chttpd_db, handle_compact_req, 2}},
    {<<"_design">>,           {chttpd_db, handle_design_req, 2}},
    {<<"_temp_view">>,        {chttpd_view, handle_temp_view_req, 2}},
    {<<"_changes">>,          {chttpd_db, handle_changes_req, 2}},
    {<<"_shards">>,           {mem3_httpd, handle_shards_req, 2}}]},
{design_url_handlers, list, [
    {<<"_view">>,             {chttpd_view, handle_view_req, 3}},
    {<<"_show">>,             {chttpd_show, handle_doc_show_req, 3}},
    {<<"_list">>,             {chttpd_show, handle_view_list_req, 3}},
    {<<"_update">>,           {chttpd_show, handle_doc_update_req, 3}},
    {<<"_info">>,             {chttpd_db, handle_design_info_req, 3}},
    {<<"_rewrite">>,          {chttpd_rewrite, handle_rewrite_req, 3}}
]}].

