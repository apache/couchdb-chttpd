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

-module(chttpd_auth_cache).
-behaviour(gen_server).
-behaviour(config_listener).

-export([start_link/0, get_user_creds/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).
-export([listen_for_changes/1, changes_callback/2]).
-export([update_auth_doc/1]).
-export([handle_config_change/5]).

-include_lib("couch/include/couch_db.hrl").

-define(ADMIN_CTX, {user_ctx, #user_ctx{roles = [<<"_admin">>]}}).
-define(CACHE, chttpd_auth_cache_lru).

-record(state, {
    changes_pid,
    last_seq="0"
}).

%% public functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_auth_doc(Doc) ->
    DbName = ?l2b(config:get("chttpd_auth", "authentication_db", "_users")),
    fabric:update_doc(DbName, Doc, []).

get_user_creds(UserName) when is_list(UserName) ->
    get_user_creds(?l2b(UserName));
get_user_creds(UserName) when is_binary(UserName) ->
    case couch_auth_cache:get_admin(UserName) of
    nil ->
        get_from_cache(UserName);
    Props ->
        case get_from_cache(UserName) of
        nil ->
            Props;
        UserProps when is_list(UserProps) ->
            couch_auth_cache:add_roles(Props,
	        couch_util:get_value(<<"roles">>, UserProps))
        end
    end.

get_from_cache(UserName) ->
    try ets_lru:lookup_d(?CACHE, UserName) of
	{ok, Props} ->
	    couch_stats:increment_counter([couchdb, auth_cache_hits]),
	    couch_log:debug("cache hit for ~s", [UserName]),
	    Props;
	_ ->
	    Props = load_user_from_db(UserName),
	    couch_stats:increment_counter([couchdb, auth_cache_misses]),
	    couch_log:debug("cache miss for ~s", [UserName]),
	    ets_lru:insert(?CACHE, UserName, Props),
	    Props
    catch
	error:badarg ->
	    couch_stats:increment_counter([couchdb, auth_cache_misses]),
	    couch_log:debug("cache miss for ~s", [UserName]),
	    load_user_from_db(UserName)
    end.

%% gen_server callbacks

init([]) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {ok, #state{changes_pid = spawn_changes(0)}}.

handle_call(restart_listener, _From, #state{changes_pid=Pid} = State) ->
    exit(Pid, kill),
    {reply, ok, State};
handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, Reason}, #state{changes_pid=Pid} = State) ->
    Seq = case Reason of
        {seq, EndSeq} ->
            EndSeq;
        _ ->
            couch_log:notice("~p changes listener died ~p", [?MODULE, Reason]),
            0
    end,
    erlang:send_after(5000, self(), {start_listener, Seq}),
    {noreply, State#state{last_seq=Seq}};
handle_info({start_listener, Seq}, State) ->
    {noreply, State#state{changes_pid = spawn_changes(Seq)}};
handle_info({gen_event_EXIT, {config_listener, ?MODULE}, _Reason}, State) ->
    erlang:send_after(5000, self(), restart_config_listener),
    {noreply, State};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{changes_pid = Pid}) ->
    exit(Pid, kill).

code_change(_OldVsn, #state{}=State, _Extra) ->
    {ok, State}.

handle_config_change("chttpd_auth", "authentication_db", _, _, _) ->
    {ok, gen_server:call(?MODULE, restart_listener, infinity)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

%% private functions

spawn_changes(Since) ->
    {Pid, _} = spawn_monitor(?MODULE, listen_for_changes, [Since]),
    Pid.

listen_for_changes(Since) ->
    CBFun = fun ?MODULE:changes_callback/2,
    Args = #changes_args{
        feed = "continuous",
        since = Since,
        heartbeat = true,
        filter = {default, main_only}
    },
    fabric:changes(dbname(), CBFun, Since, Args).

changes_callback(start, Since) ->
    {ok, Since};
changes_callback({stop, EndSeq, _Pending}, _) ->
    exit({seq, EndSeq});
changes_callback({change, {Change}}, _) ->
    UserName = username(couch_util:get_value(id, Change)),
    couch_log:debug("Invalidating cached credentials for ~s", [UserName]),
    ets_lru:remove(?CACHE, UserName),
    {ok, couch_util:get_value(seq, Change)};
changes_callback(timeout, EndSeq) ->
    exit({seq, EndSeq});
changes_callback({error, _}, EndSeq) ->
    exit({seq, EndSeq}).

load_user_from_db(UserName) ->
    try fabric:open_doc(dbname(), docid(UserName), [?ADMIN_CTX, ejson_body]) of
	{ok, Doc} ->
	    {Props} = couch_doc:to_json_obj(Doc, []),
	    Props;
	_Else ->
	    couch_log:warning("no record of user ~s", [UserName]),
	    nil
    catch error:database_does_not_exist ->
	    nil
    end.

dbname() ->
    config:get("chttpd_auth", "authentication_db", "_users").

docid(UserName) ->
    <<"org.couchdb.user:", UserName/binary>>.

username(<<"org.couchdb.user:", UserName/binary>>) ->
    UserName.
