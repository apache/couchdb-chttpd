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

-module(chttpd_handlers).

-export([
    provider/2,
    url_handler/2,
    db_handler/2,
    design_handler/2
]).

-include_lib("couch/include/couch_db.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

provider(App, Module) ->
    couch_epi_functions:childspec(chttpd_handlers_subscription,
       App, chttpd_handlers, Module).

url_handler(HandlerKey, DefaultFun) ->
    ok = ensure_enabled(HandlerKey),
    case collect(url_handler, [HandlerKey]) of
        [HandlerFun] -> HandlerFun;
        [] -> DefaultFun
    end.

db_handler(HandlerKey, DefaultFun) ->
    ok = ensure_enabled(HandlerKey),
    case collect(db_handler, [HandlerKey]) of
        [HandlerFun] -> HandlerFun;
        [] -> DefaultFun
    end.

design_handler(HandlerKey, DefaultFun) ->
    ok = ensure_enabled(HandlerKey),
    case collect(design_handler, [HandlerKey]) of
        [HandlerFun] -> HandlerFun;
        [] -> DefaultFun
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

is_disabled(KeyBin) ->
    BlacklistConfig = config:get("chttpd", "url_handler_blacklist", []),
    {ok, Blacklist} = couch_util:parse_term(BlacklistConfig),
    Key = binary_to_list(KeyBin),
    lists:member(Key, Blacklist).

 ensure_enabled(Key) ->
    case is_disabled(Key) of
      true ->
        throw("The URL endpoint " ++ Key ++ "has been disabled.");
      false ->
        ok
    end.

collect(Func, Args) ->
    Results = do_apply(Func, Args, [ignore_providers]),
    [HandlerFun || HandlerFun <- Results, HandlerFun /= no_match].

do_apply(Func, Args, Opts) ->
    Handle = couch_epi:get_handle(?MODULE),
    couch_epi:apply(Handle, chttpd, Func, Args, Opts).
