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

-module(chttpd_external).

-export([handle_external_req/2, handle_external_req/3]).
-export([send_external_response/2, json_req_obj/2, json_req_obj/3]).
-export([default_or_content_type/2, parse_external_response/1]).

-import(chttpd,[send_error/4]).

-include_lib("couch/include/couch_db.hrl").

% handle_external_req/2
% for the old type of config usage:
% _external = {chttpd_external, handle_external_req}
% with urls like
% /db/_external/action/design/name
handle_external_req(#httpd{
                        path_parts=[_DbName, _External, UrlName | _Path]
                    }=HttpReq, Db) ->
    process_external_req(HttpReq, Db, UrlName);
handle_external_req(#httpd{path_parts=[_, _]}=Req, _Db) ->
    send_error(Req, 404, <<"external_server_error">>, <<"No server name specified.">>);
handle_external_req(Req, _) ->
    send_error(Req, 404, <<"external_server_error">>, <<"Broken assumption">>).

% handle_external_req/3
% for this type of config usage:
% _action = {chttpd_external, handle_external_req, <<"action">>}
% with urls like
% /db/_action/design/name
handle_external_req(HttpReq, Db, Name) ->
    process_external_req(HttpReq, Db, Name).

process_external_req(HttpReq, Db, Name) ->

    Response = couch_external_manager:execute(binary_to_list(Name),
        json_req_obj(HttpReq, Db)),

    case Response of
    {unknown_external_server, Msg} ->
        send_error(HttpReq, 404, <<"external_server_error">>, Msg);
    _ ->
        send_external_response(HttpReq, Response)
    end.

json_req_obj(Req, Db) -> json_req_obj(Req, Db, null).
json_req_obj(#httpd{mochi_req=Req,
               method=Method,
               path_parts=Path,
               req_body=ReqBody
            }, Db, DocId) ->
    Body = case ReqBody of
        undefined ->
            MaxSize = list_to_integer(
                config:get("couchdb", "max_document_size", "4294967296")),
            Req:recv_body(MaxSize);
        Else -> Else
    end,
    ParsedForm = case Req:get_primary_header_value("content-type") of
        "application/x-www-form-urlencoded" ++ _ when Method =:= 'POST' ->
            mochiweb_util:parse_qs(Body);
        _ ->
            []
    end,
    Headers = Req:get(headers),
    Hlist = mochiweb_headers:to_list(Headers),
    {ok, Info} = fabric:get_db_info(Db),

    % add headers...
    {[{<<"info">>, {Info}},
        {<<"uuid">>, couch_uuids:new()},
        {<<"id">>, DocId},
        {<<"method">>, Method},
        {<<"path">>, Path},
        {<<"query">>, json_query_keys(to_json_terms(Req:parse_qs()))},
        {<<"headers">>, to_json_terms(Hlist)},
        {<<"body">>, Body},
        {<<"peer">>, ?l2b(Req:get(peer))},
        {<<"form">>, to_json_terms(ParsedForm)},
        {<<"cookie">>, to_json_terms(Req:parse_cookie())},
        {<<"userCtx">>, couch_util:json_user_ctx(Db)}]}.

to_json_terms(Data) ->
    to_json_terms(Data, []).
to_json_terms([], Acc) ->
    {lists:reverse(Acc)};
to_json_terms([{Key, Value} | Rest], Acc) when is_atom(Key) ->
    to_json_terms(Rest, [{list_to_binary(atom_to_list(Key)), list_to_binary(Value)} | Acc]);
to_json_terms([{Key, Value} | Rest], Acc) ->
    to_json_terms(Rest, [{list_to_binary(Key), list_to_binary(Value)} | Acc]).

json_query_keys({Json}) ->
    json_query_keys(Json, []).
json_query_keys([], Acc) ->
    {lists:reverse(Acc)};
json_query_keys([{<<"startkey">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"startkey">>, ?JSON_DECODE(Value)}|Acc]);
json_query_keys([{<<"endkey">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"endkey">>, ?JSON_DECODE(Value)}|Acc]);
json_query_keys([{<<"key">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"key">>, ?JSON_DECODE(Value)}|Acc]);
json_query_keys([{<<"descending">>, Value} | Rest], Acc) ->
    json_query_keys(Rest, [{<<"descending">>, ?JSON_DECODE(Value)}|Acc]);
json_query_keys([Term | Rest], Acc) ->
    json_query_keys(Rest, [Term|Acc]).

send_external_response(Req, Response) ->
    #extern_resp_args{
        code = Code,
        data = Data,
        ctype = CType,
        headers = Headers,
        json = Json
    } = parse_external_response(Response),
    Headers1 = default_or_content_type(CType, Headers),
    Headers2 = chttpd_cors:headers(Req, Headers1),
    case Json of
    nil ->
        chttpd:send_response(Req, Code, Headers2, Data);
    Json ->
        chttpd:send_json(Req, Code, Headers2, Json)
    end.

parse_external_response({Response}) ->
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
            {"", _} ->
                Args;
            {<<"code">>, Value} ->
                Args#extern_resp_args{code=Value};
            {<<"stop">>, true} ->
                Args#extern_resp_args{stop=true};
            {<<"json">>, Value} ->
                Args#extern_resp_args{
                    json=Value,
                    ctype="application/json"};
            {<<"body">>, Value} ->
                Args#extern_resp_args{data=Value, ctype="text/html; charset=utf-8"};
            {<<"base64">>, Value} ->
                Args#extern_resp_args{
                    data=base64:decode(Value),
                    ctype="application/binary"
                };
            {<<"headers">>, {Headers}} ->
                NewHeaders = lists:map(fun({Header, HVal}) ->
                    {couch_util:to_list(Header), couch_util:to_list(HVal)}
                end, Headers),
                Args#extern_resp_args{headers=NewHeaders};
            _ -> % unknown key
                Msg = lists:flatten(io_lib:format("Invalid data from external server: ~p", [{Key, Value}])),
                throw({external_response_error, Msg})
            end
        end, #extern_resp_args{}, Response).

default_or_content_type(DefaultContentType, Headers) ->
    IsContentType = fun({X, _}) -> string:to_lower(X) == "content-type" end,
    case lists:any(IsContentType, Headers) of
    false ->
        [{"Content-Type", DefaultContentType} | Headers];
    true ->
        Headers
    end.
