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

-module(chttpd_misc).

-export([handle_welcome_req/2,handle_favicon_req/2,handle_utils_dir_req/2,
    handle_all_dbs_req/1,handle_replicate_req/1,handle_restart_req/1,
    handle_uuids_req/1,handle_config_req/1,handle_log_req/1,
    handle_task_status_req/1,handle_sleep_req/1,handle_welcome_req/1,
    handle_utils_dir_req/1, handle_favicon_req/1, handle_system_req/1,
    handle_up_req/1]).


-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

-import(chttpd,
    [send_json/2,send_json/3,send_method_not_allowed/2,
    send_chunk/2,start_chunked_response/3]).

% httpd global handlers

handle_welcome_req(Req) ->
    handle_welcome_req(Req, <<"Welcome">>).

handle_welcome_req(#httpd{method='GET'}=Req, WelcomeMessage) ->
    send_json(Req, {[
        {couchdb, WelcomeMessage},
        {version, list_to_binary(couch_server:get_version())},
        {bigcouch, get_version()}
    ]});
handle_welcome_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

get_version() ->
    Releases = release_handler:which_releases(),
    Version = case [V || {"bigcouch", V, _, current} <- Releases] of
    [] ->
        case [V || {"bigcouch", V, _, permanent} <- Releases] of
        [] ->
            "dev";
        [Permanent] ->
            Permanent
        end;
    [Current] ->
        Current
    end,
    list_to_binary(Version).

handle_favicon_req(Req) ->
    handle_favicon_req(Req, config:get("chttpd", "docroot")).

handle_favicon_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    chttpd:serve_file(Req, "favicon.ico", DocumentRoot);
handle_favicon_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_utils_dir_req(Req) ->
    handle_utils_dir_req(Req, config:get("chttpd", "docroot")).

handle_utils_dir_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    "/" ++ UrlPath = chttpd:path(Req),
    case chttpd:partition(UrlPath) of
    {_ActionKey, "/", RelativePath} ->
        % GET /_utils/path or GET /_utils/
        chttpd:serve_file(Req, RelativePath, DocumentRoot);
    {_ActionKey, "", _RelativePath} ->
        % GET /_utils
        RedirectPath = chttpd:path(Req) ++ "/",
        chttpd:send_redirect(Req, RedirectPath)
    end;
handle_utils_dir_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_sleep_req(#httpd{method='GET'}=Req) ->
    Time = list_to_integer(chttpd:qs_value(Req, "time")),
    receive snicklefart -> ok after Time -> ok end,
    send_json(Req, {[{ok, true}]});
handle_sleep_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_all_dbs_req(#httpd{method='GET'}=Req) ->
    Args = couch_mrview_http:parse_params(Req, undefined),
    ShardDbName = config:get("mem3", "shard_db", "dbs"),
    %% shard_db is not sharded but mem3:shards treats it as an edge case
    %% so it can be pushed thru fabric
    {ok, Info} = fabric:get_db_info(ShardDbName),
    Etag = couch_httpd:make_etag({Info}),
    {ok, Resp} = chttpd:etag_respond(Req, Etag, fun() ->
        {ok, Resp} = chttpd:start_delayed_json_response(Req, 200, [{"Etag",Etag}]),
        VAcc = #vacc{req=Req,resp=Resp},
        fabric:all_docs(ShardDbName, fun all_dbs_callback/2, VAcc, Args)
    end),
    case is_record(Resp, vacc) of
        true -> {ok, Resp#vacc.resp};
        _ -> {ok, Resp}
    end;
handle_all_dbs_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

all_dbs_callback({meta, _Meta}, #vacc{resp=Resp0}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "["),
    {ok, Acc#vacc{resp=Resp1}};
all_dbs_callback({row, Row}, #vacc{resp=Resp0}=Acc) ->
    Prepend = couch_mrview_http:prepend_val(Acc),
    case couch_util:get_value(id, Row) of <<"_design", _/binary>> ->
        {ok, Acc};
    DbName ->
        {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, [Prepend, ?JSON_ENCODE(DbName)]),
        {ok, Acc#vacc{prepend=",", resp=Resp1}}
    end;
all_dbs_callback(complete, #vacc{resp=Resp0}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_chunk(Resp0, "]"),
    {ok, Resp2} = chttpd:end_delayed_json_response(Resp1),
    {ok, Acc#vacc{resp=Resp2}};
all_dbs_callback({error, Reason}, #vacc{resp=Resp0}=Acc) ->
    {ok, Resp1} = chttpd:send_delayed_error(Resp0, Reason),
    {ok, Acc#vacc{resp=Resp1}}.

handle_task_status_req(#httpd{method='GET'}=Req) ->
    {Replies, _BadNodes} = gen_server:multi_call(couch_task_status, all),
    Response = lists:flatmap(fun({Node, Tasks}) ->
        [{[{node,Node} | Task]} || Task <- Tasks]
    end, Replies),
    send_json(Req, lists:sort(Response));
handle_task_status_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_replicate_req(#httpd{method='POST', user_ctx=Ctx} = Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    %% see HACK in chttpd.erl about replication
    PostBody = get(post_body),
    try replicate(PostBody, Ctx, mem3_rep_manager) of
    {ok, {continuous, RepId}} ->
        send_json(Req, 202, {[{ok, true}, {<<"_local_id">>, RepId}]});
    {ok, {cancelled, RepId}} ->
        send_json(Req, 200, {[{ok, true}, {<<"_local_id">>, RepId}]});
    {ok, {JsonResults}} ->
        send_json(Req, {[{ok, true} | JsonResults]});
    {ok, stopped} ->
        send_json(Req, 200, {[{ok, stopped}]});
    {error, {Type, Details}} ->
        send_json(Req, 500, {[{error, Type}, {reason, Details}]});
    {error, not_found} ->
        send_json(Req, 404, {[{error, not_found}]});
    {error, Reason} ->
        try
            send_json(Req, 500, {[{error, Reason}]})
        catch
        exit:{json_encode, _} ->
            send_json(Req, 500, {[{error, couch_util:to_binary(Reason)}]})
        end
    catch
    throw:{db_not_found, Msg} ->
        send_json(Req, 404, {[{error, db_not_found}, {reason, Msg}]});
    throw:{unauthorized, Msg} ->
        send_json(Req, 404, {[{error, unauthorized}, {reason, Msg}]})
    end;
handle_replicate_req(Req) ->
    send_method_not_allowed(Req, "POST").

replicate({Props} = PostBody, Ctx, Module) ->
    Node = choose_node([
        couch_util:get_value(<<"source">>, Props),
        couch_util:get_value(<<"target">>, Props)
    ]),
    case rpc:call(Node, couch_rep, replicate, [PostBody, Ctx, Module]) of
    {badrpc, Reason} ->
        erlang:error(Reason);
    Res ->
        Res
    end.

choose_node(Key) when is_binary(Key) ->
    Checksum = erlang:crc32(Key),
    Nodes = lists:sort([node()|erlang:nodes()]),
    lists:nth(1 + Checksum rem length(Nodes), Nodes);
choose_node(Key) ->
    choose_node(term_to_binary(Key)).

handle_restart_req(#httpd{method='POST'}=Req) ->
    couch_server_sup:restart_core_server(),
    send_json(Req, 200, {[{ok, true}]});
handle_restart_req(Req) ->
    send_method_not_allowed(Req, "POST").


handle_uuids_req(Req) ->
    couch_httpd_misc_handlers:handle_uuids_req(Req).


% Config request handler


% GET /_config/
% GET /_config
handle_config_req(#httpd{method='GET', path_parts=[_]}=Req) ->
    Grouped = lists:foldl(fun({{Section, Key}, Value}, Acc) ->
        case dict:is_key(Section, Acc) of
        true ->
            dict:append(Section, {list_to_binary(Key), list_to_binary(Value)}, Acc);
        false ->
            dict:store(Section, [{list_to_binary(Key), list_to_binary(Value)}], Acc)
        end
    end, dict:new(), config:all()),
    KVs = dict:fold(fun(Section, Values, Acc) ->
        [{list_to_binary(Section), {Values}} | Acc]
    end, [], Grouped),
    send_json(Req, 200, {KVs});
% GET /_config/Section
handle_config_req(#httpd{method='GET', path_parts=[_,Section]}=Req) ->
    KVs = [{list_to_binary(Key), list_to_binary(Value)}
            || {Key, Value} <- config:get(Section)],
    send_json(Req, 200, {KVs});
% PUT /_config/Section/Key
% "value"
handle_config_req(#httpd{method='PUT', path_parts=[_, Section, Key]}=Req) ->
    Value = chttpd:json_body(Req),
    Persist = chttpd:header_value(Req, "X-Couch-Persist") /= "false",
    OldValue = config:get(Section, Key, ""),
    ok = config:set(Section, Key, ?b2l(Value), Persist),
    send_json(Req, 200, list_to_binary(OldValue));
% GET /_config/Section/Key
handle_config_req(#httpd{method='GET', path_parts=[_, Section, Key]}=Req) ->
    case config:get(Section, Key, null) of
    null ->
        throw({not_found, unknown_config_value});
    Value ->
        send_json(Req, 200, list_to_binary(Value))
    end;
% DELETE /_config/Section/Key
handle_config_req(#httpd{method='DELETE',path_parts=[_,Section,Key]}=Req) ->
    Persist = chttpd:header_value(Req, "X-Couch-Persist") /= "false",
    case config:get(Section, Key, null) of
    null ->
        throw({not_found, unknown_config_value});
    OldValue ->
        config:delete(Section, Key, Persist),
        send_json(Req, 200, list_to_binary(OldValue))
    end;
handle_config_req(Req) ->
    send_method_not_allowed(Req, "GET,PUT,DELETE").

% httpd log handlers

handle_log_req(#httpd{method='GET'}=Req) ->
    Bytes = list_to_integer(chttpd:qs_value(Req, "bytes", "1000")),
    Offset = list_to_integer(chttpd:qs_value(Req, "offset", "0")),
    Chunk = couch_log:read(Bytes, Offset),
    {ok, Resp} = start_chunked_response(Req, 200, [
        % send a plaintext response
        {"Content-Type", "text/plain; charset=utf-8"},
        {"Content-Length", integer_to_list(length(Chunk))}
    ]),
    send_chunk(Resp, Chunk),
    send_chunk(Resp, "");
handle_log_req(Req) ->
    send_method_not_allowed(Req, "GET").

% Note: this resource is exposed on the backdoor interface, but it's in chttpd
% because it's not couch trunk
handle_system_req(Req) ->
    Other = erlang:memory(system) - lists:sum([X || {_,X} <-
        erlang:memory([atom, code, binary, ets])]),
    Memory = [{other, Other} | erlang:memory([atom, atom_used, processes,
        processes_used, binary, code, ets])],
    {NumberOfGCs, WordsReclaimed, _} = statistics(garbage_collection),
    {{input, Input}, {output, Output}} = statistics(io),
    send_json(Req, {[
        {uptime, element(1,statistics(wall_clock)) div 1000},
        {memory, {Memory}},
        {run_queue, statistics(run_queue)},
        {ets_table_count, length(ets:all())},
        {context_switches, element(1, statistics(context_switches))},
        {reductions, element(1, statistics(reductions))},
        {garbage_collection_count, NumberOfGCs},
        {words_reclaimed, WordsReclaimed},
        {io_input, Input},
        {io_output, Output},
        {os_proc_count, couch_proc_manager:get_proc_count()},
        {process_count, erlang:system_info(process_count)},
        {process_limit, erlang:system_info(process_limit)},
        {message_queues, message_queues(registered())},
        {internal_replication_jobs, mem3_sync:get_backlog()},
        {distribution, {get_distribution_stats()}}
    ]}).

get_distribution_stats() ->
    lists:map(fun({Node, Socket}) ->
        {ok, Stats} = inet:getstat(Socket),
        {Node, {Stats}}
    end, erlang:system_info(dist_ctrl)).

handle_up_req(#httpd{method='GET'} = Req) ->
    case config:get("couchdb", "maintenance_mode") of
    "true" ->
        send_json(Req, 404, {[{status, maintenance_mode}]});
    _ ->
        send_json(Req, 200, {[{status, ok}]})
    end;

handle_up_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

message_queues(Registered) ->
    Queues = lists:map(fun(Name) ->
        Type = message_queue_len,
        {Type, Length} = process_info(whereis(Name), Type),
        {Name, Length}
    end, Registered),
    {Queues}.
