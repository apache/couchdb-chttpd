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

-module(chttpd_db_test).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    TmpDb = ?tempdb(),
    Addr = config:get("chttpd", "bind_address", "127.0.0.1"),
    Port = mochiweb_socket_server:get(chttpd, port),
    Url = lists:concat(["http://", Addr, ":", Port, "/", ?b2l(TmpDb)]),
    create_db(Url),
    Url.

teardown(Url) ->
    delete_db(Url).

create_db(Url) ->
    {ok, _, _, _} = test_request:put(Url,
                [{"Content-Type", "application/json"}], "{}").
delete_db(Url) ->
    {ok, _, _, _} = test_request:delete(Url).

create_doc(Url, Id) ->
    test_request:put(Url ++ "/" ++ Id,
        [{"Content-Type", "application/json"}], "{\"mr\": \"rocko artischockbert\"}").

all_test_() ->
    {
        "chttpd db tests",
        {
            setup,
            fun chttpd_test_util:start_couch/0, fun chttpd_test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_return_ok_true_on_bulk_update/1,
                    fun should_send_headers_for_downloading_docs_if_query_param_passed/1,
                    fun should_not_send_headers_for_downloading_docs_if_no_query_param_passed/1,
                    fun should_send_headers_for_downloading_ddocs_if_query_param_passed/1,
                    fun should_not_send_headers_for_downloading_ddocs_if_no_query_param_passed/1
                ]
            }
        }
    }.


should_return_ok_true_on_bulk_update(Url) ->
    ?_assertEqual(true,
        begin
            {ok, _, _, Body} = create_doc(Url, "testdoc"),
            {Json} = ?JSON_DECODE(Body),
            Ref = couch_util:get_value(<<"rev">>, Json, undefined),
            NewDoc = "{\"docs\": [{\"_rev\": \"" ++ ?b2l(Ref) ++ "\", \"_id\": \"testdoc\"}]}",
            {ok, _, _, ResultBody} = test_request:post(Url ++ "/_bulk_docs/",
                [{"Content-Type", "application/json"}], NewDoc),
            ResultJson = ?JSON_DECODE(ResultBody),
            {InnerJson} = lists:nth(1, ResultJson),
            couch_util:get_value(<<"ok">>, InnerJson, undefined)
        end).

should_send_headers_for_downloading_docs_if_query_param_passed(Url) ->
    ?_assertEqual("attachment",
        begin
            {ok, _, _, _} = create_doc(Url, "testdoc"),
            {ok, _, Headers, _} = test_request:get(Url ++ "/testdoc?download=true"),
            proplists:get_value("Content-Disposition", Headers)
        end).

should_not_send_headers_for_downloading_docs_if_no_query_param_passed(Url) ->
    ?_assertEqual(undefined,
        begin
            {ok, _, _, _} = create_doc(Url, "testdoc"),
            {ok, _, Headers, _} = test_request:get(Url ++ "/testdoc"),
            proplists:get_value("Content-Disposition", Headers)
        end).

should_send_headers_for_downloading_ddocs_if_query_param_passed(Url) ->
    ?_assertEqual("attachment",
        begin
            {ok, _, _, _} = create_doc(Url, "_design/testdoc"),
            {ok, _, Headers, _} = test_request:get(Url ++ "/_design/testdoc?download=true"),
            proplists:get_value("Content-Disposition", Headers)
        end).

should_not_send_headers_for_downloading_ddocs_if_no_query_param_passed(Url) ->
    ?_assertEqual(undefined,
        begin
            {ok, _, _, _} = create_doc(Url, "_design/testdoc"),
            {ok, _, Headers, _} = test_request:get(Url ++ "/_design/testdoc"),
            proplists:get_value("Content-Disposition", Headers)
        end).
