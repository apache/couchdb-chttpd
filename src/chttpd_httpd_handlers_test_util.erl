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

-module(chttpd_httpd_handlers_test_util).

-export([endpoints_test/3]).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

%%%=========================================================================
%%%  Test environment defintions
%%%=========================================================================

setup("mocked") ->
    fun setup_mocked/1;
setup("not_mocked") ->
    fun setup_not_mocked/1;
setup("skip") ->
    fun(_) -> undefined end.

setup_mocked({Endpoint, {_Path, Module, Function}}) ->
    catch meck:unload(Module),
    meck:new(Module, [passthrough, non_strict]),
    Expected = mock_handler(Endpoint, Module, Function),
    Expected.

setup_not_mocked({_Endpoint, {_Path, Module, _Function}}) ->
    catch meck:unload(Module),
    meck:new(Module, [non_strict]),
    undefined.

teardown({_Endpoint, {Module, _F, _A}}, _) ->
    catch meck:unload(Module),
    ok.

endpoints_test(App, Module, Apps) ->
    {
        "Checking dynamic endpoints",
        {
            setup,
            fun() -> test_util:start_couch(Apps) end,
            fun test_util:stop/1,
            %% we use instantiator to postpone test instantiation
            %% so we can detect endpoint overrides
            fun(_) -> make_tests(App, Module, [
                    {"mocked", url_handler},
                    {"mocked", db_handler},
                    {"mocked", design_handler},
                    {"not_mocked", url_handler},
                    {"not_mocked", db_handler},
                    {"not_mocked", design_handler}
                ])
            end
        }
    }.

check_dynamic_endpoints(Setup, EndpointType, App, Module) ->
    {Specs, Skips} = get_handlers(EndpointType, Module),
    TestMessage = "Checking '"
            ++ atom_to_list(App) ++ " -- "
            ++ atom_to_list(EndpointType)
            ++ "' [" ++ Setup ++ "] dynamic endpoints",
    {
        TestMessage, [
            make_test_case(Setup, EndpointType, Spec) || Spec <- Specs
        ] ++ [
            make_test_case("skip", EndpointType, Spec) || Spec <- Skips
        ]
    }.

make_test_case(Setup, EndpointType, {Path, Module, Function}) ->
    {
        lists:flatten(io_lib:format("~s -- \"~s\"", [EndpointType, ?b2l(Path)])),
        {
            foreachx, setup(Setup), fun teardown/2,
            [
                {{EndpointType, {Path, Module, Function}}, select_test(Setup)}
            ]
        }
    }.


make_tests(App, Module, Casses) ->
    [
        check_dynamic_endpoints(Setup, EndpointType, App, Module)
            || {Setup, EndpointType} <- Casses
    ].

select_test("mocked") -> fun ensure_called/2;
select_test("not_mocked") -> fun verify_we_fail_if_missing/2;
select_test("skip") -> fun ensure_skip_overridden/2.

mock_handler(url_handler = Endpoint, M, F) ->
    meck:expect(M, F, fun(X) -> {return, Endpoint, X} end),
    fun M:F/1;
mock_handler(db_handler = Endpoint, M, F) ->
    meck:expect(M, F, fun(X, Y) -> {return, Endpoint, X, Y} end),
    fun M:F/2;
mock_handler(design_handler = Endpoint, M, F) ->
    meck:expect(M, F, fun(X, Y, Z) -> {return, Endpoint, X, Y, Z} end),
    fun M:F/3.

%%%=========================================================================
%%%  Test functions definitions
%%%=========================================================================

ensure_skip_overridden(_, _) ->
    ?_assert(true).

ensure_called({url_handler = Endpoint, {Path, _M, _Fun}}, ExpectedFun) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assertEqual(ExpectedFun, HandlerFun),
        ?assertMatch({return, Endpoint, x}, HandlerFun(x))
     end);
ensure_called({db_handler = Endpoint, {Path, _M, _Fun}}, ExpectedFun) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assertEqual(ExpectedFun, HandlerFun),
        ?assertMatch({return, Endpoint, x, y}, HandlerFun(x, y))
     end);
ensure_called({design_handler = Endpoint, {Path, _M, _Fun}}, ExpectedFun) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assertEqual(ExpectedFun, HandlerFun),
        ?assertMatch({return, Endpoint, x, y, z}, HandlerFun(x, y, z))
     end).

%% Test the test: when the final target function is missing,
%% the Fun call must fail.
verify_we_fail_if_missing({url_handler = Endpoint, {Path, _M, _Fun}}, _) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assert(is_function(HandlerFun)),
        ?assertError(undef, HandlerFun(x))
    end);
verify_we_fail_if_missing({db_handler = Endpoint, {Path, _M, _Fun}}, _) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assert(is_function(HandlerFun)),
        ?assertError(undef, HandlerFun(x, y))
    end);
verify_we_fail_if_missing({design_handler = Endpoint, {Path, _M, _Fun}}, _) ->
    HandlerFun = handler(Endpoint, Path),
    ?_test(begin
        ?assert(is_function(HandlerFun)),
        ?assertError(undef, HandlerFun(x, y, z))
    end).

%%%=========================================================================
%%%  Internal functions definitions
%%%=========================================================================

handler(EndpointType, HandlerKey) ->
    chttpd_handlers:EndpointType(HandlerKey, default_handler(EndpointType)).

get_active_handler(EndpointType, HandlerKey) ->
    Info = erlang:fun_info(handler(EndpointType, HandlerKey)),
    {
         HandlerKey,
         proplists:get_value(module, Info),
         proplists:get_value(name, Info)
    }.

default_handler(url_handler) -> fun chttpd_db:handle_request/1;
default_handler(db_handler) -> fun chttpd_db:db_req/2;
default_handler(design_handler) -> fun chttpd_db:bad_action_req/3.

get_handlers(EndpointType, Module) ->
    Handlers = Module:handlers(EndpointType),
    lists:partition(fun(Spec) ->
        is_active(EndpointType, Spec)
    end, Handlers).

is_active(EndpointType, {Path, _Module, _Function} = Spec) ->
    get_active_handler(EndpointType, Path) == Spec.
