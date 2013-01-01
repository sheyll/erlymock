%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
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

invalid_parameter_1_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,
                                         fun(B) ->
                                                 B == b
                                         end]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertMatch({'EXIT',
                  {{case_clause,
                   {unexpected_function_parameter,
                    {error_in_parameter, 1}, {expected, a}, {actual, 666},
                    {invokation, some_mod, some_fun, [666, b], _}}},
                   _}},
                 catch some_mod:some_fun(666, b)).

invalid_order_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,b]),
    em:strict(M, some_mod, some_fun, [a]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertMatch({'EXIT',
                  {{case_clause,
                    {unexpected_invokation,
                     {actual,{invokation,some_mod,some_fun,[a], _}},
                     {expected,{expectation,_Ref, some_mod,some_fun,[a,b],
                                {return,ok},
                                _Listeners}}}}, _}},
                 catch some_mod:some_fun(a)).

too_many_invokations_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,b]),
    em:replay(M),
    ok = some_mod:some_fun(a, b),
    process_flag(trap_exit, true),
    ?assertMatch({'EXIT', {{case_clause,{unexpected_invokation,
                                         {actual,
                                          {invokation,some_mod,some_fun,[a, b], _}}}}, _}},
                 catch some_mod:some_fun(a, b)).

invokations_missing_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,b]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertError({badmatch,
                  {invokations_missing,
                   [{expectation,_, some_mod,some_fun,[a,b],{return,ok}, _}]}},
                 em:verify(M)).

invalid_parameter_2_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,
                                         fun(B) ->
                                                 B == b
                                         end]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertMatch({'EXIT', {{case_clause,
                            {unexpected_function_parameter,
                             {error_in_parameter, 2},
                             {expected, _},
                             {actual, 666},
                             {invokation, some_mod, some_fun, [a, 666], _}}}, _}},
                 catch(some_mod:some_fun(a, 666))).

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

gen_fsm_unimplemented_stops_test() ->
    ?assertEqual({stop, normal, state}, em:handle_info(x, y, state)),
    ?assertEqual({stop, normal, ok, state}, em:handle_sync_event(x, y, z, state)),
    ?assertEqual({stop, normal, state}, em:handle_event(x, y, state)),
    ?assertEqual({ok, state_name, state}, em:code_change(old_vsn, state_name, state, extra)).

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

error_module_already_locked_test() ->
    process_flag(trap_exit, true),
    M1 = em:new(),
    em:strict(M1, xxx,y,[]),
    em:replay(M1),

    M2 = em:new(),
    em:strict(M2, xxx, y, []),
    try em:replay(M2) of
        _NoError ->
            throw(expected_module_already_locked_error)
    catch
        exit:{{bad_return_value,
               {em_error_module_already_mocked, xxx}}, _} ->
            pass;
        C:E ->
            throw({expected_module_already_locked_error_but_got, C, E})
    end,
    xxx:y(),
    em:verify(M1).


verify_missing_invokation_test() ->
    process_flag(trap_exit, true),
    M = em:new(),
    em:strict(M, xxx,y,[]),
    em:replay(M),
    em:verify(M).
