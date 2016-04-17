% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_db_doc_size_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(USER, "chttpd_db_test_admin").
-define(PASS, "pass").
-define(AUTH, {basic_auth, {?USER, ?PASS}}).
-define(CONTENT_JSON, {"Content-Type", "application/json"}).

setup() ->
    ok = config:set("admins", ?USER, ?PASS, _Persist=false),
    ok = config:set("couchdb", "single_max_doc_size", "50"),
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    Url.

teardown(Url) ->
    delete_db(Url),
    ok = config:delete("admins", ?USER, _Persist=false),
    ok = config:delete("couchdb", "single_max_doc_size").

create_db(Url) ->
    {ok, Status, _, _} = test_request:put(Url, [?CONTENT_JSON, ?AUTH], "{}"),
    ?assert(Status =:= 201 orelse Status =:= 202).

delete_db(Url) ->
    {ok, 200, _, _} = test_request:delete(Url, [?AUTH]).

all_test_() ->
    {
        "chttpd db single doc max size test",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun post_single_doc/1,
                    fun put_single_doc/1,
                    fun bulk_doc/1
                ]
            }
        }
    }.

post_single_doc(Url) ->
    ?_assertEqual({<<"error">>, <<"too_large">>},
        begin
            NewDoc = "{\"post_single_doc\": \"some_doc\",
                \"_id\": \"testdoc\", \"should_be\" : \"too_large\"}",
            {ok, _, _, ResultBody} = test_request:post(Url,
                [?CONTENT_JSON, ?AUTH], NewDoc),
            {ErrorMsg} = ?JSON_DECODE(ResultBody),
            lists:nth(1, ErrorMsg)
        end).

put_single_doc(Url) ->
    ?_assertEqual({<<"error">>, <<"too_large">>},
        begin
            NewDoc = "{\"post_single_doc\": \"some_doc\",
                \"_id\": \"testdoc\", \"should_be\" : \"too_large\"}",
            {ok, _, _, ResultBody} = test_request:put(Url ++ "/" ++ "testid",
                [?CONTENT_JSON, ?AUTH], NewDoc),
            {ErrorMsg} = ?JSON_DECODE(ResultBody),
            lists:nth(1, ErrorMsg)
        end).

bulk_doc(Url) ->
    NewDoc = "{\"docs\": [{\"doc1\": 1}, {\"errordoc\":
        \"this_should_be_the_error_document\"}]}",
    {ok, _, _, ResultBody} = test_request:post(Url ++ "/_bulk_docs/",
        [?CONTENT_JSON, ?AUTH], NewDoc),
    ResultJson = ?JSON_DECODE(ResultBody),
    {InnerJson1} = lists:nth(1, ResultJson),
    {InnerJson2} = lists:nth(2, ResultJson),
    Error = couch_util:get_value(<<"error">>, InnerJson1),
    Msg = couch_util:get_value(<<"error">>, InnerJson2),
    ?_assertEqual(<<"too_large">>, Error),
    ?_assertEqual(undefined, Msg).
