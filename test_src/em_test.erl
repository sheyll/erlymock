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

invokation_timout_test() ->
    Pid = spawn_link(fun() ->
                             M = em:new(),
                             em:strict(M, some_mod, some_fun, [a]),
                             em:replay(M),
                             receive ok -> ok end
                     end),
    process_flag(trap_exit, true),
    receive
        {'EXIT', 
         Pid, 
         {invokation_timeout,
          {missing_invokations,
           [{expectation,some_mod,some_fun,[a],{return,ok}}]}}} ->
            ok 
    end.

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
                     {expected,{expectation,some_mod,some_fun,[a,b],
                                {return,ok}}}}}, _}}, 
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

auto_lock_test() ->
    process_flag(trap_exit, true),
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
    process_flag(trap_exit, true),
    Target = self(),
    spawn(fun() ->
                  M1 = em:new(),
                  em:strict(M1, mmod1, test, [x]),
                  em:replay(M1),
                  receive after 1 -> ok end,
                  mmod1:test(x),
                  em:verify(M1),
                  Target ! mm1
          end),
    spawn(fun() ->
                  M2 = em:new(),
                  em:lock(M2, [mmod1, mmod3]),
                  em:strict(M2, mmod2, test, [y,z]),
                  em:replay(M2),
                  receive after 1 -> ok end,
                  mmod2:test(y,z),
                  em:verify(M2),
                  Target ! mm2
          end),
    receive 
        mm1 ->            
            receive 
                mm2 ->
                    ok
            end
    end.

em_zelf_test() ->        
    M = em:new(),
    em:strict(M, mod, f, [em:zelf()]),
    em:replay(M),
    mod:f(self()),
    em:verify(M).
