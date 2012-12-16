%%%=============================================================================
%%%                                        
%%%               |  ° __   _|  _  __  |_   _       _ _   (TM)
%%%               |_ | | | (_| (/_ | | |_) (_| |_| | | | 
%%%
%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%% @author Tobias Schlager <tobias.schlager@lindenbaum.eu>
%%% @author Timo Koepke <timo.koepke@lindenbaum.eu>
%%% @author Olle Toernstroem  <olle.toernstroem@lindenbaum.eu>
%%% @copyright (C) 2011, Lindenbaum GmbH
%%%
%%%-----------------------------------------------------------------------------

-module(em_module_locker_test).

-include_lib("eunit/include/eunit.hrl").

-define(SLEEP(TIMEOUT), receive after TIMEOUT -> ok end).

%%%=============================================================================
%%% TESTS
%%%=============================================================================
remove_lock_after_some_time_test() ->
    process_flag(trap_exit, true),
    case whereis(em_module_locker) of
        undefined -> 
            ok;
        MLP ->
            link(MLP),
            exit(MLP, shutdown),
            receive 
                {'EXIT', MLP, _} ->
                    ok
            end
    end,
    TestPid = spawn(fun() -> receive A -> A end end),
    em_module_locker:lock(TestPid, [m2]),
    receive after 4400 -> ok end,
    TestPid2 = spawn(fun() -> receive A -> A end end),
    em_module_locker:lock(TestPid2, [m2]),
    TestPid2 ! die.
    


simple_lock_and_unlock_test() ->
    process_flag(trap_exit, true),
    case whereis(em_module_locker) of
        undefined -> 
            ok;
        MLP ->
            link(MLP),
            exit(MLP, shutdown),
            receive 
                {'EXIT', MLP, _} ->
                    ok
            end
    end,
    Test = self(),
    MockPid1 = spawn_link(fun() ->                
                                  receive A -> A end 
                          end),
    em_module_locker:lock(MockPid1, [m1,m2]),
    MockTemp = spawn_link(fun() -> 
                                  em_module_locker:lock(self(), [m2]),
                                  Test ! mock_temp_locked
                          end),
    _MockPid2 = spawn_link(fun() ->
                                  em_module_locker:lock(self(), [m2]),
                                  Test ! m2_locked,
                                  receive A -> A end
                          end),
    receive 
        m2_locked -> 
            throw(fail1)
    after 10 ->
            ok
    end,
    exit(MockTemp, kill),
    receive
        {'EXIT', MockTemp, _} ->
            ok
    end,
    MockPid1 ! shutdown,
    receive 
        m2_locked -> 
            ok
    after 10 ->
            throw(fail2)
    end,
    receive 
        mock_temp_locked ->
            throw(fail3)
    after 10 ->
        ok
    end.
    


%%%=============================================================================
%%% Internal Functions
%%%=============================================================================
