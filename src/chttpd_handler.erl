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

%% @doc Configurable, dynamic creation of endpoint handler callback indirections

-module(chttpd_handler).

-export([url_handler/1, db_url_handlers/0, design_url_handlers/0,
    build/0, build/1]).

% debugging
-export([test_cfg/0]).

-vsn(5).

%% @doc a complete configuration data set
-type config() :: [Function::{Name::atom(), clauses|list, [bind()]}].

%% @doc one essential pair of a pattern and the fun to be returned for it
-type bind() :: {Endpoint::term(), MFA::{atom(), atom(), integer()}}.

-spec url_handler(Endpoint::list()) -> Handler::fun().
%% @doc Dispatch endpoint to fun, wrapper to hide dynamic module.
url_handler(Endpoint) ->
    try 
        chttpd_dyn_handler:url_handler(Endpoint)
    catch
        _:_ ->
        build(),
        chttpd_dyn_handler:url_handler(Endpoint)
    end.

-spec db_url_handlers() -> [{Endpoint::list(), Handler::fun()}].
%% @doc Get a list of endpoints and handler funs, wrapper to hide dyn module.
db_url_handlers() ->
    chttpd_dyn_handler:db_url_handlers().

-spec design_url_handlers() -> [{Endpoint::list(), Handler::fun()}].
%% @doc Get a list of endpoints and handler funs, wrapper to hide dyn module.
design_url_handlers() ->
    chttpd_dyn_handler:design_url_handlers().

-spec build() -> ok | [].
%% @doc Create the dynamic handler functions from ini file.
build() ->
    build(load_defs()).

-spec build(HandlerCfg::config()) -> ok.
%% @doc Compile the complete syntax tree, purge and load the dynamic module
build(Cfg) when is_list(Cfg) ->
    io:format(user, "~n➽ Dynamic handlers using config: ~n~p~n", [Cfg]),
    Opts = [verbose, report_errors],
    {ok, Mod, Bin} = compile:forms(forms(chttpd_dyn_handler, Cfg), Opts),
    % don't code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
    ok.

-spec load_defs() -> CombinedHandlerCfg::config().
%% @doc assemble the configuration from the chttpd_handler.cfg of all apps.
load_defs() ->
    Apps = application:loaded_applications(),
    io:format(user, "~n➽ Dynamic handler module parses these apps' configs: ~n~p~n", [Apps]),
    {AllURLHandlers, AllDBHandlers, AllDesignHandlers} = lists:foldl(
        fun(App, {URLHandlers, DBHandlers, DesignHandlers}) ->
            Defs = load_defs(App),
            {URLHandlers   ++ [ B || {docs, clauses, B}  <- Defs ],
            DBHandlers     ++ [ B || {db, list, B} <- Defs ],
            DesignHandlers ++ [ B || {design, list, B} <- Defs ]}
        end,
        {[],[],[]},
        [element(1, A) || A <- Apps]),
    [{url_handler, clauses, lists:flatten(AllURLHandlers)},
    {db_url_handlers, list, lists:flatten(AllDBHandlers)},
    {design_url_handlers, list, lists:flatten(AllDesignHandlers)}].

-spec load_defs(AppName::atom()) -> OneAppsHandlerCfg::config().
%% @doc assemble the configuration from the chttpd_handler.cfg of all apps.
% load_defs(App) ->
%    [ check_def(test_cfg(), "no path (testing)") ].
load_defs(App) ->
io:format(user, "~n➽ Dynamic handler uses priv dir for ~p: ~p~n", [App, code:priv_dir(App)]),    
case code:priv_dir(App) of
        {error, _Error} ->
            [];
        Dir ->
            Path = Dir ++ "/chttpd_handler.cfg",
            case file:consult(Path) of
                {ok, Defs} ->
                    [ check_def(Def, Path) || Def <- Defs ];
                {error, _Error} ->
                    []
            end
    end.

check_def({_, _, []}=Def, Path) ->
    throw({no_defs_error, Def, Path});
check_def({docs, clauses, B}, Path) ->
    {docs, clauses, sort(check_dupes(check_bindings(B, list, 1, Path), Path))};
check_def({db, list, B}, Path) ->
    {db, list, check_dupes(check_bindings(B, binary, 2, Path), Path)};
check_def({design, list, B}, Path) ->
    {design, list, check_dupes(check_bindings(B, binary, 3, Path), Path)};
check_def(Def, Path) ->
    throw({tag_error, Def, Path}).

check_bindings([{Endpoint, {M, F, Arity}}=Good | More], list, Arity, Path)
    when is_list(Endpoint), is_atom(M), is_atom(F), is_integer(Arity) ->
    [Good | check_bindings(More, list, Arity, Path)];
check_bindings([{Endpoint, {M, F, Arity}}=Good | More], binary, Arity, Path)
    when is_binary(Endpoint), is_atom(M), is_atom(F), is_integer(Arity) ->
    [Good | check_bindings(More, binary, Arity, Path)];
check_bindings([{'_', {M, F, Arity}}=Good | More], Type, Arity, Path)
    when is_atom(M), is_atom(F), is_integer(Arity) ->
    [Good | check_bindings(More, Type, Arity, Path)];
check_bindings([Bad | _], _, Arity, Path) ->
    throw({syntax_or_arity_error, Bad, exptected_arity, Arity, Path});
check_bindings([], _, _, _) ->
    [].

-spec sort(Cfg::config()) -> config().
%% @doc make sure that any _ is the last clause of a generated function
sort(Cfg) ->
    lists:sort(
        fun ({'_',_}, _) -> false;
            (_, {'_',_}) -> true;
            (_, _) -> true
        end,
        Cfg).

-spec check_dupes(Cfg::config(), Path::list()) -> config() | [].
%% @doc crash if an endpoint is defined twice
check_dupes(Cfg, Path) ->
    lists:sort(
        fun ({E,_}=Def1, {E, _}=Def2) ->
                throw({duplicate_endpoint, E, Def1, Def2, Path});
            (_, _) -> true
        end,
        Cfg).

-spec forms(Mod::atom(), Defs::[bind()]) -> erl_syntax:syntaxTree().
%% @doc The complete syntax tree of the dynamic module
forms(Mod, Defs) ->
    Statements = [
        module_stmt(Mod),
        export_stmt(
            [{Name, case Lay of clauses -> 1; list -> 0 end}
            || {Name, Lay, _} <- Defs ])
        | [ binding_function(Name, Lay, Def) || {Name, Lay, Def} <- Defs ]],
    io:format(user, "~n➽ Dynamic handler statement tree: ~n~p~n", [Statements]),
    [ erl_syntax:revert(X) || X <- Statements].

-spec module_stmt(ModuleName::atom()) -> erl_syntax:syntaxTree().
%% @doc Create syntax tree for the module statement of the dynamic module
module_stmt(Name) ->
    erl_syntax:attribute(
        erl_syntax:atom(module),
        [erl_syntax:atom(Name)]).

-spec export_stmt([Exports::{Name::atom(), Arity::integer()}]) ->
    erl_syntax:syntaxTree().
%% @doc Create syntax tree for the export statement of the dynamic module
export_stmt(Exports) ->
    erl_syntax:attribute(
        erl_syntax:atom(export),
        [erl_syntax:list(
            [ erl_syntax:arity_qualifier(
              erl_syntax:atom(Name),
              erl_syntax:integer(Arity))
            || {Name, Arity} <- Exports ]
    )]).

-spec binding_function(Name::atom(), clauses | list, [bind()]) ->
    erl_syntax:syntaxTree().
%% @doc Create syntax subtree for a function that either has multiple clauses
%% or returns a list of tuples of a tag and a fun.
%/ binding_function(Name, list, []) ->
%/    erl_syntax:function(
%/        erl_syntax:atom(Name),
%/        [erl_syntax:clause([], none,
%/            [erl_syntax:list([])])]);
binding_function(Name, list, Defs) ->
    erl_syntax:function(
        erl_syntax:atom(Name),
        [erl_syntax:clause([], none,
            [erl_syntax:list(
            [erl_syntax:tuple([
                erl_syntax:abstract(P),
                create_fun(Def)])
            || {P, Def} <- Defs ])])]);
binding_function(Name, clauses, []) ->
    io:format(user, "~n➽ Dynamic handler module creates no url handlers.~n", []),
    erl_syntax:function(
        erl_syntax:atom(Name),
        [erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(nil)])]);
binding_function(Name, clauses, Defs) ->
    erl_syntax:function(
        erl_syntax:atom(Name),
        [create_fun_clause(Def) || Def <- sort(Defs)]).

-spec create_fun_clause(bind()) -> erl_syntax:syntaxTree().
%% @doc Create syntax subtree for a function clause with one implicit fun call.
create_fun_clause({P, MFA}) ->
    erl_syntax:clause(
        [case P of
            '_' -> erl_syntax:underscore();
             _  -> erl_syntax:abstract(P)
        end],
        none,
        [create_fun(MFA)]).

-spec create_fun(MFA::{Module::atom(), Function::atom(), Arity::integer()}) ->
    erl_syntax:syntaxTree().
%% @doc Create syntax subtree for an implicit fun call.
create_fun({M, F, A}) ->
    erl_syntax:implicit_fun(
        erl_syntax:atom(M),
        erl_syntax:atom(F),
        erl_syntax:integer(A)).

test_cfg() ->
{docs, clauses, [
    {"",                      {chttpd_misc, handle_welcome_req, 1}},
    {"favicon.ico",           {chttpd_misc, handle_favicon_req, 1}},
    {"_utils",                {chttpd_misc, handle_utils_dir_req, 1}},
    {"_all_dbs",              {chttpd_misc, handle_all_dbs_req, 1}},
    {"_active_tasks",         {chttpd_misc, handle_task_status_req, 1}},
    {"_node",                 {chttpd_misc, handle_node_req, 1}},
    {"_reload_query_servers", {chttpd_misc,
        handle_reload_query_servers_req, 1}},
    {"_replicate",            {chttpd_misc, handle_replicate_req, 1}},
    {"_uuids",                {chttpd_misc, handle_uuids_req, 1}},
    {"_session",              {chttpd_auth, handle_session_req, 1}},
    {"_up",                   {chttpd_misc, handle_up_req, 1}},
    {"_user",                 {chttpd_auth, handle_user_req, 1}},
    {'_',                     {chttpd_db,   handle_request, 1}}
]}.

