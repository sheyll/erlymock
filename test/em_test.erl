%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2011, Sven Heyll
%%%-------------------------------------------------------------------
-module(em_test).
-include_lib("eunit/include/eunit.hrl").


simple_strict_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,
                                         fun(B) ->
                                                 B == b
                                         end]),
    em:strict(M, some_mod, some_fun_bad, [],
             {function, fun(_) -> throw(xxx) end}),
    em:strict(M, some_mod, some_fun, [a,
                                         fun(B) ->
                                                 B == c
                                         end]),
    em:strict(M, some_mod, some_other_fun, [a]),
    em:strict(M, some_other_mod, some_other_fun, [], {return, ok2}),
    em:replay(M),
    ok = some_mod:some_fun(a, b),
    ?assertEqual(xxx, (catch some_mod:some_fun_bad())),
    ok = some_mod:some_fun(a, c),
    ok = some_mod:some_other_fun(a),
    ok2 = some_other_mod:some_other_fun(),
    em:verify(M).

invokation_timout_test() ->
    Pid = spawn_link(fun() ->
                             M = em:new(),
                             em:strict(M, some_mod, some_fun, [a]),
                             em:replay(M, 1),
                             receive never_to_receive -> ok end
                     end),
    process_flag(trap_exit, true),
    receive
        {'EXIT',
         Pid,
         Reason} ->
            ?assertMatch({invokation_timeout,
                          {missing_invokations,
                           [{expectation,
                             _Ref,
                             _Group,
                             some_mod,some_fun,[a],
                             {return,ok},
                             _Listeners}]}},
                         Reason)
    end.

invalid_parameter_1_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,
                                      fun(B) ->
                                              B == b
                                      end]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertExit({mock_error,
                 {unexpected_invokation, _,
                  [{parameter_mismatch,
                    {parameter, 1},
                    {expected,  a},
                    {actual,    666}, _}]}},
                some_mod:some_fun(666, b)),
    ?assertError({unexpected_invokation, _, _},
                 em:verify(M)).

invalid_order_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,b]),
    em:strict(M, some_mod, some_fun, [a]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertExit({mock_error,
                 {unexpected_invokation,
                  {invokation, some_mod, some_fun, [a], _},
                  _}},
                 some_mod:some_fun(a)),
    ?assertError({unexpected_invokation, _, _},
                 em:verify(M)).

too_many_invokations_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,b]),
    em:replay(M),
    ok = some_mod:some_fun(a, b),
    process_flag(trap_exit, true),
    ?assertExit({mock_error,
                 {unexpected_invokation,
                  {invokation, some_mod, some_fun, [a,b], _}}},
                 some_mod:some_fun(a,b)),
    ?assertError({unexpected_invokation, _}, em:verify(M)).

invokations_missing_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,b]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertError({invokations_missing, [_]},
                 em:verify(M)).

invalid_parameter_2_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,
                                      fun(B) ->
                                              B == b
                                      end]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertExit({mock_error,
                 {unexpected_invokation, _,
                  [{parameter_mismatch,
                    {parameter, 2},
                    {expected,  _},
                    {actual,    666}, _}]}},
                some_mod:some_fun(a, 666)),
    ?assertError({unexpected_invokation, _, _},
                 em:verify(M)).

strict_and_stub_test() ->
    M = em:new(),
    em:stub(M, some_modx, some_fun, [a, b, em:any()], {return, ok}),
    em:strict(M, some_mod, some_fun, [a, b], {function,
                                                 fun(Args) -> Args end}),
    em:replay(M),
    ok = some_modx:some_fun(a, b, c),
    [a, b] = some_mod:some_fun(a, b),
    ok = some_modx:some_fun(a, b, c),
    ok = some_modx:some_fun(a, b, c),
    ok = some_modx:some_fun(a, b, random:uniform(123)),
    em:verify(M).

stub_only_test() ->
    M = em:new(),
    em:stub(M, some_modx, some_fun, [a, b, c], {return, ok123}),
    em:stub(M, some_mody, some_fun, [a, b, c]),
    em:stub(M, some_modz, some_fun, [a, b, c], {function, fun(_) -> well end}),
    em:replay(M),
    well = some_modz:some_fun(a, b, c),
    ok123 = some_modx:some_fun(a, b, c),
    ok = some_mody:some_fun(a, b, c),
    ok123 = some_modx:some_fun(a, b, c),
    em:verify(M).

fun_answer_test() ->
    M = em:new(),
    em:stub(M, some_modx, some_fun, [a, b, c],
            {function, fun([a,b,c]) ->
                               answer
                       end}),
    em:replay(M),
    answer = some_modx:some_fun(a, b, c),
    em:verify(M).

empty_stub_test() ->
    M = em:new(),
    em:stub(M, some_modx, some_fun, [a, a, c], {return, ok}),
    em:replay(M),
    em:verify(M).

empty_test() ->
    M = em:new(),
    em:replay(M),
    em:verify(M).

check_arguments_test() ->
    M = em:new(),
    ?assertException(error, function_clause, em:stub(M,x,y,[],bad)),
    ?assertException(error, function_clause, em:strict(M,x,y,[],{function1, also_bad})),
    em:replay(M),
    em:verify(M).

em_statem_callback_code_change_test() ->
    ?assertEqual({ok, state_name, state}, em:code_change(old_vsn, state_name, state, extra)).

em_statem_callback_bad_requests_ignore_test() ->
    ?assertEqual({keep_state, state, {reply, from, {error,{bad_request, programming, x}}}}, em:programming({call, from}, x, state)),
    ?assertEqual({keep_state, state, {reply, from, {error,{bad_request, no_expectations, x}}}}, em:no_expectations({call, from}, x, state)),
    ?assertEqual({keep_state, state, {reply, from, {error,{bad_request, replaying, x}}}}, em:replaying({call, from}, x, state)),
    ?assertEqual({keep_state, state, {reply, from, {error,{bad_request, deranged, x}}}}, em:deranged({call, from}, x, state)).

nothing_test() ->
    {module, _} = code:ensure_loaded(mnesia),
    M = em:new(),
    em:nothing(M, mnesia),
    em:replay(M),
    ?assertMatch({'EXIT', {undef, _}}, catch mnesia:blub(some_arg)),
    ?assertMatch(ok, em:verify(M)).

em_zelf_test() ->
    M = em:new(),
    em:strict(M, mod, f, [em:zelf()]),
    em:replay(M),
    mod:f(self()),
    em:verify(M).

await_expectations_noexpectations_test() ->
    M = em:new(),
    Test = self(),
    em:strict(M, mod, f, [],
             {function, fun(_) -> Test ! go_on end}),
    em:replay(M),
    mod:f(),
    receive go_on -> ok end,
    em:await_expectations(M).

await_expectations_test() ->
    M = em:new(),
    Test = self(),
    em:strict(M, mod, f, [],
             {function, fun(_) -> Test ! go_on end}),
    em:replay(M),
    spawn(fun() -> receive after 200 -> mod:f() end end),
    em:await_expectations(M).

await_test() ->
    M = em:new(),
    F1 = em:strict(M, mod, f1, [arg1, arg2], {return, ret}),
    F2 = em:strict(M, mod, f2, []),
    em:strict(M, mod, f3, []),
    em:replay(M),
    mod:f1(arg1, arg2),
    OtherPid = spawn(fun() -> receive after 200 -> mod:f2() end end),
    %% ... and block until it happens
    ?assertEqual({success, OtherPid, []},
                 em:await(M, F2)),
    %% in the invokation has already happened em:await shall return
    %% immediately with the historic invokation details...
    ?assertEqual({success, self(), [arg1, arg2]},
                 em:await(M, F1)),
    mod:f3(),
    ?assertEqual({error, invalid_handle}, em:await(M, xxx)),
    em:verify(M).

error_module_already_mocked_test() ->
    process_flag(trap_exit, true),
    M1 = em:new(),
    em:strict(M1, xxx,y,[]),
    em:replay(M1),

    M2 = em:new(),
    em:strict(M2, xxx, y, []),
    try em:replay(M2) of
        _NoError ->
            throw(expected_module_already_mocked_error)
    catch
        exit:{{bad_return_value,
               {em_error_module_already_mocked, xxx}}, _} ->
            pass;
        C:E ->
            throw({expected_module_already_mocked_error_but_got, C, E})
    end,
    xxx:y(),
    em:verify(M1).

verify_missing_invokation_test() ->
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, xxx,y,[]),
    em:replay(M),
    ?assertError({badmatch, {invokations_missing, _}}, em:verify(M)).

await_no_expectations_test() ->
    M = em:new(),
    F = em:strict(M, mod, f1, []),
    em:replay(M),
    mod:f1(),
    ?assertEqual({success, self(), []},
                 em:await(M, F)),
    em:verify(M).

two_groups_test() ->
    M = em:new(),

    em:strict(M, m1, f1, []),

    [G1, G2] = em:new_groups(M, [g1, g2]),
    em:strict(G1, g1, f1, []),
    em:strict(G1, g1, f2, []),
    em:strict(G2, g2, f1, []),
    em:strict(G2, g2, f2, []),

    em:strict(M, m1, f2, []),
    em:replay(M),

    m1:f1(),

    g2:f1(),
    g1:f1(),
    g1:f2(),
    g2:f2(),

    m1:f2(),

    em:await_expectations(M).

two_groups2_test() ->
    M = em:new(),

    em:strict(M, m1, f1, []),

    [G1, G2] = em:new_groups(M, [g1, g2]),
    em:strict(G1, g1, f1, []),
    em:strict(G1, g1, f2, []),
    em:strict(G2, g2, f1, []),
    em:strict(G2, g2, f2, []),

    em:replay(M),

    m1:f1(),

    g2:f1(),
    g1:f1(),
    g2:f2(),
    g1:f2(),

    em:await_expectations(M).

mock_deranged_invalid_invokation_test() ->
    M = em:new(),

    em:strict(M, mod, f1, [right]),
    em:strict(M, mod, f2, [left]),
    em:strict(M, mod, f3, []),

    em:replay(M),

    mod:f1(right),

    ?assertExit({mock_error,
                 {unexpected_invokation,
                  {invokation,
                   mod, f2, [right], _},
                  _}},
                mod:f2(right)),

    ?assertExit({mock_error, mock_deranged},
                 mod:f1(wrong)),

    ?assertExit({mock_error, mock_deranged},
                 mod:f3()),

    ?assertError({unexpected_invokation,
                  {invokation,
                   mod, f2, [right], _},
                  _},
                 em:verify(M)).

mock_deranged_no_expectation_test() ->
    M = em:new(),

    em:strict(M, mod, f1, [right]),

    em:replay(M),

    mod:f1(right),

    MockError = {unexpected_invokation,
                  {invokation,
                   mod, f1, [left], self()}},

    ?assertExit({mock_error, MockError},
                mod:f1(left)),

    ?assertExit({mock_error, mock_deranged},
                mod:f1(wrong)),

    ?assertError(MockError,
                 em:verify(M)).


mock_deranged_stub_no_expectation_test() ->
    M = em:new(),

    em:stub(M, mod, fstub, []),
    em:strict(M, mod, f1, [right]),

    em:replay(M),

    mod:f1(right),

    MockError = {unexpected_invokation,
                  {invokation,
                   mod, f1, [left], self()}},
    ?assertExit({mock_error, MockError},
                mod:f1(left)),

    ?assertExit({mock_error, mock_deranged},
                mod:fstub()),

    ?assertError(MockError,
                 em:verify(M)).

mock_deranged_stub_test() ->
    M = em:new(),

    em:stub(M, mod, fstub, []),
    em:strict(M, mod, f1, [right]),
    em:strict(M, mod, f2, [right]),

    em:replay(M),

    mod:f1(right),

    ?assertExit({mock_error,
                 {unexpected_invokation,
                  {invokation,
                   mod, f1, [left], _},
                 _}},
                mod:f1(left)),

    ?assertExit({mock_error, mock_deranged},
                mod:fstub()),

    ?assertError({unexpected_invokation,
                  {invokation,
                   mod, f1, [left], _},
                 _},
                 em:verify(M)).

mock_deranged_await_test() ->
    M = em:new(),

    Already = em:stub(M, mod, fstub, []),
    em:strict(M, mod, f1, [right]),
    NotYet = em:strict(M, mod, f2, [right]),

    em:replay(M),

    mod:f1(right),

    ?assertExit({mock_error,
                 {unexpected_invokation,
                  {invokation,
                   mod, f1, [left], _},
                 _}},
                mod:f1(left)),

    ?assertEqual({error, mock_deranged},
                 em:await(M, Already)),

    ?assertEqual({error, mock_deranged},
                 em:await(M, NotYet)),

    ?assertError({unexpected_invokation,
                  {invokation,
                   mod, f1, [left], _},
                  _},
                 em:verify(M)).

mock_deranged_await_expectations_test() ->
    M = em:new(),

    em:strict(M, mod, f1, [right]),
    em:strict(M, mod, f2, [left]),
    em:strict(M, mod, f3, []),

    em:replay(M),

    mod:f1(right),

    ?assertExit({mock_error,
                 {unexpected_invokation,
                  {invokation,
                   mod, f2, [right], _},
                  _}},
                mod:f2(right)),

    ?assertExit({mock_error, mock_deranged},
                 mod:f1(wrong)),

    ?assertExit({mock_error, mock_deranged},
                 mod:f3()),

    ?assertError({unexpected_invokation,
                  {invokation,
                   mod, f2, [right], _},
                  _},
                 em:await_expectations(M)).


generate_call_log_test() ->
	?debugHere,
	process_flag(trap_exit, true),
	M = em:new(),
	em:strict(M, some_mod, a_fun, [fun is_atom/1]),
	em:strict(M, some_mod, ret_fun, [fun is_atom/1], {function, fun(_A) -> yes end}),
	em:strict(M, some_mod, a_fun, [fun is_atom/1]),
	em:stub(M, some_mod, stub_fun, [a, b]),
	em:replay(M),
	some_mod:stub_fun(a,b),
	some_mod:a_fun(this_is_an_atom),
	some_mod:ret_fun(not_me),
	some_mod:a_fun(this_is_one_too),
	some_mod:stub_fun(a,b),

	?assertMatch([{some_mod, stub_fun, [a,b],{return, ok}},
				  {some_mod, a_fun, [this_is_an_atom],{return, ok}},
				  {some_mod, ret_fun, [not_me],{function, FuncTerm}},
				  {some_mod, a_fun, [this_is_one_too],{return, ok}},
				  {some_mod, stub_fun, [a,b],{return, ok}}] when is_function(FuncTerm),
				 em:call_log(M)),
		em:verify(M).
