% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_httpd_handlers_tests).

-export([handlers/1]).

-include_lib("couch/include/couch_eunit.hrl").

handlers(url_handler) ->
    [
        {<<"">>, chttpd_misc, handle_welcome_req},
        {<<"favicon.ico">>, chttpd_misc, handle_favicon_req},
        {<<"_utils">>, chttpd_misc, handle_utils_dir_req},
        {<<"_all_dbs">>, chttpd_misc, handle_all_dbs_req},
        {<<"_active_tasks">>, chttpd_misc, handle_task_status_req},
        {<<"_node">>, chttpd_misc, handle_node_req},
        {<<"_reload_query_servers">>, chttpd_misc, handle_reload_query_servers_req},
        {<<"_replicate">>, chttpd_misc, handle_replicate_req},
        {<<"_uuids">>, chttpd_misc, handle_uuids_req},
        {<<"_session">>, chttpd_auth, handle_session_req},
        {<<"_up">>, chttpd_misc, handle_up_req},
        {<<"anything">>, chttpd_db, handle_request}
    ];
handlers(db_handler) ->
    [
        {<<"_view_cleanup">>, chttpd_db, handle_view_cleanup_req},
        {<<"_compact">>, chttpd_db, handle_compact_req},
        {<<"_design">>, chttpd_db, handle_design_req},
        {<<"_temp_view">>, chttpd_view, handle_temp_view_req},
        {<<"_changes">>, chttpd_db, handle_changes_req}
    ];
handlers(design_handler) ->
    [
        {<<"_view">>, chttpd_view, handle_view_req},
        {<<"_show">>, chttpd_show, handle_doc_show_req},
        {<<"_list">>, chttpd_show, handle_view_list_req},
        {<<"_update">>, chttpd_show, handle_doc_update_req},
        {<<"_info">>, chttpd_db, handle_design_info_req},
        {<<"_rewrite">>, chttpd_rewrite, handle_rewrite_req}
    ].

chttpd_endpoints_test_() ->
    Apps = [couch_epi, chttpd],
    chttpd_httpd_handlers_test_util:endpoints_test(chttpd, ?MODULE, Apps).
