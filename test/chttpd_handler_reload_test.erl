% Licensed under the Apache License,  Version 2.0 (the "License");  you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%                 http://www.apache.org/licenses/LICENSE-2.0
%
% Unless  required  by  applicable  law  or  agreed  to  in  writing,  software
% distributed  under the  License  is distributed on an "AS IS" BASIS,  WITHOUT
% WARRANTIES  OR  CONDITIONS  OF  ANY  KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_handler_reload_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% for testing

a() -> a.
b() -> b.
c() -> c.
d() -> d.
e() -> e.
f() -> f.
g() -> g.
h() -> h.

-spec reload_test() -> ok.
%% @doc verifies that there are no side effects from prior builds.
reload_test() ->

    % undo any previously loaded test builds

    code:purge(chttpd_dyn_handler), % (must for delete)
    code:delete(chttpd_dyn_handler),
    ?assertError(undef, _F0 = chttpd_dyn_handler:clause_test("A")),

    % first build

    Cfg =
        [{clause_test, clauses, [
            {"A",     {chttpd_handler_reload_test, a, 0}},
            {"B",     {chttpd_handler_reload_test, b, 0}},
            {'_',     {chttpd_handler_reload_test, c, 0}}
            ]},
        {list_test, list, [
            {<<"1">>,     {chttpd_handler_reload_test, a, 0}},
            {<<"2">>,     {chttpd_handler_reload_test, b, 0}}
            ]}],

    chttpd_handler:build(Cfg),

    F1 = chttpd_dyn_handler:clause_test("A"),
    a = F1(),

    F2 = chttpd_dyn_handler:clause_test("B"),
    b = F2(),

    F3 = chttpd_dyn_handler:clause_test(xxx),
    c = F3(),

    {_, F4} = lists:keyfind(<<"1">>, 1, chttpd_dyn_handler:list_test()),
    a = F4(),

    {_, F5} = lists:keyfind(<<"2">>, 1, chttpd_dyn_handler:list_test()),
    b = F5(),

    % second build, expected to overwrite the earlier

    Cfg2 =
        [{clause_test, clauses, [
            {"D",     {chttpd_handler_reload_test, d, 0}},
            {"E",     {chttpd_handler_reload_test, e, 0}},
            {'_',     {chttpd_handler_reload_test, f, 0}}
            ]},
        {list_test, list, [
            {<<"3">>,     {chttpd_handler_reload_test, g, 0}},
            {<<"4">>,     {chttpd_handler_reload_test, h, 0}}
            ]}],

    chttpd_handler:build(Cfg2),

    F6 = chttpd_dyn_handler:clause_test("A"),
    io:format("~p~n", [F6]),
    ?assertError({badmatch, f}, a = F6()),

    F7 = chttpd_dyn_handler:clause_test("B"),
    ?assertError({badmatch, f}, b = F7()),

    F8 = chttpd_dyn_handler:clause_test("D"),
    d = F8(),

    F9 = chttpd_dyn_handler:clause_test("E"),
    e = F9(),

    F10 = chttpd_dyn_handler:clause_test(yyy),
    f = F10(),

    false = lists:keyfind(<<"1">>, 1, chttpd_dyn_handler:list_test()),

    false = lists:keyfind(<<"2">>, 1, chttpd_dyn_handler:list_test()),

    {_, F11} = lists:keyfind(<<"3">>, 1, chttpd_dyn_handler:list_test()),
    g = F11(),

    {_, F12} = lists:keyfind(<<"4">>, 1, chttpd_dyn_handler:list_test()),
    h = F12(),

    ok.
