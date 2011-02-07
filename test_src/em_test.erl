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
    em:stub(M, some_modx, some_fun, [a, b, c], {return, ok}),
    em:replay(M),
    ok = some_modx:some_fun(a, b, c),
    ok = some_modx:some_fun(a, b, c),
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
