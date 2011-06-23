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
    em:strict(M, some_mod, some_fun, [a, 
                                         fun(B) ->
                                                 B == c
                                         end]),
    em:strict(M, some_mod, some_other_fun, [a]),
    em:strict(M, some_other_mod, some_other_fun, [], {return, ok2}),
    em:replay(M),
    ok = some_mod:some_fun(a, b),
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
    ?assertError({case_clause, 
                  {unexpected_function_parameter,
                   {error_in_parameter, 1}, {expected, a}, {actual, 666},
                   {invokation, some_mod, some_fun, [666, b]}}}, 
                 some_mod:some_fun(666, b)).

invalid_order_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,b]),
    em:strict(M, some_mod, some_fun, [a]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertError({case_clause,{unexpected_invokation,
                               {actual,{invokation,some_mod,some_fun,[a]}},
                               {expected,{expectation,some_mod,some_fun,[a,b],
                                          {return,ok}}}}}, 
                 some_mod:some_fun(a)).

too_many_invokations_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,b]),
    em:replay(M),
    ok = some_mod:some_fun(a, b),
    process_flag(trap_exit, true),
    ?assertError({case_clause,{unexpected_invokation,
                               {actual,{invokation,some_mod,some_fun,[a, b]}}}}, 
                 some_mod:some_fun(a, b)).

invokations_missing_test() ->
    M = em:new(),
    em:strict(M, some_mod, some_fun, [a,b]),
    em:replay(M),
    process_flag(trap_exit, true),
    ?assertError({badmatch,
                  {invokations_missing,
                   [{expectation,some_mod,some_fun,[a,b],{return,ok}}]}},
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
                             {invokation, some_mod, some_fun, [a, 666]}}}, _}}, 
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
    {module, _} = code:ensure_loaded(module_not_to_call),
    M = em:new(),
    em:nothing(M, module_not_to_call),
    em:replay(M),
    ?assertMatch({'EXIT', {undef, _}}, catch module_not_to_call:some_fun(some_arg)),
    ?assertMatch(ok, em:verify(M)).

auto_lock_test() ->
    case whereis(em_module_locker) of
        undefined -> 
            ok;
        MLP ->
            process_flag(trap_exit, true),
            link(MLP),
            exit(MLP, shutdown),
            receive 
                {'EXIT', MLP, _} ->
                    ok
            end
    end,
    Target = self(),
    spawn(fun() ->
                  M1 = em:new(),
                  em:strict(M1, mod1, test, [x]),
                  em:replay(M1),
                  receive after 1 -> ok end,
                  mod1:test(x),
                  em:verify(M1),
                  Target ! m1    
          end),
    spawn(fun() ->
                  M2 = em:new(),
                  em:strict(M2, mod1, test, [y,z]),
                  em:replay(M2),
                  receive after 1 -> ok end,
                  mod1:test(y,z),
                  em:verify(M2),
                  Target ! m2
          end),
    receive 
        m1 ->
            receive 
                m2 ->
                    ok
            end
    end.

explicit_lock_test() ->
    case whereis(em_module_locker) of
        undefined -> 
            ok;
        MLP ->
            process_flag(trap_exit, true),
            link(MLP),
            exit(MLP, shutdown),
            receive 
                {'EXIT', MLP, _} ->
                    ok
            end
    end,
    Target = self(),
    M1 = spawn(fun() ->
                       M1 = em:new(),
                       em:strict(M1, mod1, test, [x]),
                       em:replay(M1),
                       receive after 1 -> ok end,
                       mod1:test(x),
                       Target ! mm1,
                       receive 
                           m1_down ->
                               ok
                       end,
                       em:verify(M1)
               end),
    spawn(fun() ->
                  M2 = em:new(),
                  em:lock(M2, [mod1, mod3]),
                  em:strict(M2, mod2, test, [y,z]),
                  em:replay(M2),
                  receive after 1 -> ok end,
                  mod2:test(y,z),
                  em:verify(M2),
                  Target ! mm2
          end),
    receive 
       mm1 ->            
            receive 
                mm2 ->
                    throw(fail1)
            after 50 ->
                    M1 ! m1_down,
                    receive
                        mm2 -> 
                            ok
                    end
            end
    end.

        
