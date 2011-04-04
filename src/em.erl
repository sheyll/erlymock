%%%-----------------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%% @copyright (C) 2011, Sven Heyll
%%% @doc
%%% The module name 'em' stands for 'Erly Mock'.
%%%
%%% <p>This mocking library works similar to Easymock.</p>
%%% 
%%% <p>After a mock process is started by <code>new/0</code> it can be
%%% programmed to expect function calls and to react to them in two 
%%% ways: <ul><li>by returning a value</li><li>by executing an arbitrary 
%%% function</li></ul> 
%%% This is done with <code>strict/4, strict/5, stub/4, stub/5</code>.
%%% </p>
%%%
%%% <p>Before the code under test is executed, the mock must be told
%%% that the programming phase is over by <code>replay/1</code>.</p>
%%%
%%% <p>In the next phase the code under test is run, and might or 
%%% might not call the functions mocked.
%%% The mock process checks that all functions programmed with 
%%% <code>strict/4, strict/5</code>are called in the
%%% correct order, with the expected arguments and reacts in the way
%%% defined during the programming phase. If a mocked function is called 
%%% although another function was expected, or if an expected function
%%% was called with different arguments, the mock process dies and
%%% prints a comprehensive error message before failing the test.</p>
%%%
%%% <p>At the end of a unit test <code>verify/1</code> is called to
%%% check for missing invocations at the end of the programming phase
%%% and to remove all modules, that were dynamically created and loaded
%%% during the programming phase.</p>
%%%
%%% NOTE: This library works by purging the modules mocked and replacing 
%%% them with dynamically created and compiled code, so be careful what
%%% you mock, i.e. it brings chaos to mock modules from kernel. This also
%%% implies, that tests that mock the same modules must be run sequentially.
%%%
%%% Apart from that, it is very advisable to <b>only mock owned modules</b>
%%% anyway.
%%% 
%%% @end
%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2011 Sven Heyll
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%% 
%%%-----------------------------------------------------------------------------

-module(em).

-behaviour(gen_fsm).

%% public API ---
-export([new/0,
         strict/4,
         strict/5,
         stub/4,
         stub/5,
         replay/1,
         verify/1,
         any/0,
	 nothing/2]).

%% gen_fsm callbacks ---
-export([programming/3,
         replaying/3,
         no_expectations/3,
         terminate/3,
         init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4]).

%% !!!NEVER CALL THIS FUNCTION!!! ---
-export([invoke/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% important types
%%

%%------------------------------------------------------------------------------
%% The type that defines the argument list passed to strict() or stub().
%% Each list element is either a value that will be matched to the actual value
%% of the parameter at that position, or a predicate function which will be 
%% applied to the actual argument.
%%------------------------------------------------------------------------------
-type args() :: [ fun((any()) ->
                             true | false)
                     | term()].

%%------------------------------------------------------------------------------
%% The type that defines the response to a mocked function call. A response is
%% either that a value is returned, or the application of a function to the 
%% actual arguments.
%%------------------------------------------------------------------------------
-type answer() :: {function, fun(([any()]) -> any())}
                  | {return, any()} .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% API
%%

%%------------------------------------------------------------------------------
%% @doc
%% Spawns a linked mock process and returns it's pid. This is usually the first
%% thing to do in each unit test. The resulting pid is used in the other
%% functions below. NOTE: only a single mock proccess is required for a single
%% unit test case. One mock process can mock an arbitrary number of different 
%% modules.
%% When the mock process dies, all uploaded modules are purged from the code
%% server, and all cover compiled modules are restored.
%% When the process that started the mock exits, the mock automatically cleans
%% up and exits.
%% After new() the mock is in 'programming' state. 
%% @end
%%------------------------------------------------------------------------------
-spec new() ->
                 pid().
new() ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
    Pid.

%%------------------------------------------------------------------------------
%% @doc
%% Adds an expectation during the programming phase for a specific functiion
%% invokation. 
%% <p>All expectations defined by 'strict' define an order in which the
%% application must call the mocked functions, hence the name 'strict' as oposed
%% to 'stub' (see below).</p>
%% <p>The parameters are:
%% <ul>
%% <li><code>M</code> the mock pid, returned by <code>new/0</code></li>
%% <li><code>Mod</code> the module of the function to mock</li>
%% <li><code>Fun</code> the name of the function to mock</li>
%% <li><code>Args</code> a list of expected arguments. 
%% Each list element is either a value that will be matched to the actual value 
%% of the parameter at that position, or a predicate function which will be 
%% applied to the actual argument.</li>
%% </ul></p>
%% <p>
%% The return value, that the application will get when calling the mocked
%% function is simply the atom <code>ok</code>. This differentiates this
%% function from <code>strict/5</code>, which allows the definition of a
%% custom response function or a custom return value.
%% </p>
%% NOTE: This function may only be called between <code>new/0</code> and
%% <code>replay/1</code> - that is during the programming phase.
%% @end
%%------------------------------------------------------------------------------
-spec strict(pid(), atom(), atom(), args()) ->
                    ok.
strict(M, Mod, Fun, Args)
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    strict(M, Mod, Fun, Args, {return, ok}).

%%------------------------------------------------------------------------------
%% @doc
%% This function behaves like <code>strict/4</code>
%% and additionally accepts a return value or an answer function. That parameter
%% <code>Answer</code> may be:
%% <ul>
%% <li><code>{return, SomeValue}</code> This causes the mocked function invocation to 
%% return the specified value.</li>
%% <li><code>{function, fun(([Arg1, ... , ArgN]) -> SomeValue)}</code> This defines
%% a function to be called when the mocked invokation happens. 
%% That function is applied to all captured actual arguments.  For convenience these
%% are passed as a list, so the user can simply write <code>fun(_) -> ...</code> 
%% when the actual values are not needed.
%% The function will be executed by the process that calls the mocked function, not
%% by the mock process. Hence the function may access <code>self()</code> and may 
%% throw an exception, which will then correctly appear in the process under test,
%% allowing unit testing of exception handling.
%% Otherwise the value returned by the function is passed through as the value 
%% returned from the invocation.
%% </li>
%% </ul>
%% @see strict/4. <code>strict/4</code>
%% @end
%%------------------------------------------------------------------------------
-spec strict(pid(), atom(), atom(), args(), answer()) ->
                    ok.
strict(M, Mod, Fun, Args, Answer = {return, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {strict, Mod, Fun, Args, Answer});
strict(M, Mod, Fun, Args, Answer = {function, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {strict, Mod, Fun, Args, Answer}).

%%------------------------------------------------------------------------------
%% @doc
%% Defines a what happens when a function is called whithout recording any
%% expectations. The invocations defined by this function may happen in any order
%% any number of times. The way, the invocation is defined is analog to
%% @see strict/4. <code>strict/4</code>
%% @end
%%------------------------------------------------------------------------------
-spec stub(pid(), atom(), atom(), args()) ->
                  ok.
stub(M, Mod, Fun, Args)
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    stub(M, Mod, Fun, Args, {return, ok}).
    
%%------------------------------------------------------------------------------
%% @doc
%% This is similar <code>stub/4</code> except that it, like
%% <code>strict/5</code> allows the definition of a return value
%% or an answer function.
%% @see stub/4. <code>stub/4</code>
%% @see strict/5. <code>strict/5</code>
%% @end
%%------------------------------------------------------------------------------
-spec stub(pid(), atom(), atom(), args(), answer()) ->
                  ok.
stub(M, Mod, Fun, Args, Answer = {return, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {stub, Mod, Fun, Args, Answer});
stub(M, Mod, Fun, Args, Answer = {function, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {stub, Mod, Fun, Args, Answer}).

%%------------------------------------------------------------------------------
%% @doc
%% This is used to express the expectation that no function of a certain module
%% is called. This will cause each function call on a module to throw an 'undef'
%% exception.
%% @end
%%------------------------------------------------------------------------------
-spec nothing(pid(), atom()) ->
		     ok.
nothing(M, Mod) when is_pid(M), is_atom(Mod) ->
   ok = gen_fsm:sync_send_event(M, {nothing, Mod}).

%%------------------------------------------------------------------------------
%% @doc
%% Finishes the programming phase and switches to the replay phase where the 
%% actual code under test may run and invoke the functions mocked. This may
%% be called only once, and only in the programming phase. This also loads 
%% (or replaces) the modules of the functions mocked.
%% In the replay phase the code under test may call all mocked functions.
%% If the application calls a mocked function with invalid arguments, or
%% if the application calls a function not expected on a mocked module, the mock
%% process dies and - if used in a typical edoc test suite - fails the test.
%% @end
%%------------------------------------------------------------------------------
replay(M) ->
    ok = gen_fsm:sync_send_event(M, replay).

%%------------------------------------------------------------------------------
%% @doc
%% Finishes the replay phase. If the code under test did not cause all expected
%% invokations defined by <code>strict/4</code> or <code>strict/5</code>, the 
%% call will fail with <code>badmatch</code> with a comprehensive error message.
%% Otherwise the mock process exits normally, returning <code>ok</code>.
%% @end
%%------------------------------------------------------------------------------
verify(M) ->
    ok = gen_fsm:sync_send_event(M, verify).

%%------------------------------------------------------------------------------
%% @doc
%% Utility function that can be used as a match function in an argument list
%% to match any value.
%% @end
%%------------------------------------------------------------------------------
-spec any() ->
                 fun((any()) ->
                    true).
any() ->
    fun(_) ->
            true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% internal state
%%

-record(expectation,
        {m :: atom(),
         f :: atom(),
         a :: args(),
         answer :: answer()}).

-record(state, {
          strict :: [#expectation{}],
          stub  :: [#expectation{}],
	  blacklist :: [atom()],
          mocked_modules :: [{atom(), {just, term()}|nothing}]
         }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% gen_fsm callbacks
%%i

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
init([]) ->
    {ok,
     programming,
     #state{
       strict = [],
       stub = [],
       blacklist = [],
       mocked_modules = []}}.


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
programming({strict, Mod, Fun, Args, Answer},
            _From,
            State = #state{strict = Strict}) ->
    {reply,
     ok,
     programming,
     State#state{
       strict = [#expectation{m = Mod, f = Fun, a = Args, answer = Answer}|Strict]}};

programming({stub, Mod, Fun, Args, Answer},
            _From,
            State = #state{stub = Stub}) ->
    {reply,
     ok,
     programming,
     State#state{
       stub = [#expectation{m = Mod, f = Fun, a = Args, answer = Answer}|Stub]}};

programming({nothing, Mod},
            _From,
            State = #state{blacklist = BL}) ->
    {reply,
     ok,
     programming,
     State#state{
       blacklist = [Mod | BL]}};	   

programming(replay,
            _From,
            State = #state{strict = Strict}) ->
    MMs = install_mock_modules(State),
    {reply,
     ok,
     case Strict of
         [] -> no_expectations;
         _ -> replaying
     end,
     State#state{
       strict = lists:reverse(Strict),
       mocked_modules = MMs}}.

%%------------------------------------------------------------------------------
%% @private (goto-char 1)
%%------------------------------------------------------------------------------
replaying(I = {invokation, Mod, Fun, Args},
          _From,
          State = #state{
            strict = [#expectation{
                        m      = Mod,
                        f      = Fun,
                        a      = EArgs,
                        answer = Answer}
                      |Rest]})
  when length(EArgs) == length(Args) ->
    case check_args(Args, EArgs) of
        true ->
            {reply,
             Answer,
             case Rest of
                 [] -> no_expectations;
                 _ -> replaying
             end,
             State#state{strict=Rest}};
        {error, Index, Expected, Actual} ->
            Reason = {unexpected_function_parameter,
                      {error_in_parameter, Index},
                      {expected, Expected},
                      {actual, Actual},
                      I},
            {stop, Reason, Reason, State}
    end;

replaying(I = {invokation, _M, _F, _A},
          _From,
          State = #state{
	    strict = [E|_]}) ->
    case handle_stub_invokation(I, State#state.stub) of
	{ok, Answer} ->
	    {reply, Answer, replaying, State};
	
	error ->
	    Reason = {unexpected_invokation, {actual, I}, {expected, E}},
	    {stop, Reason, Reason, State}
    end;

replaying(verify,
          _From,
          State) ->
    Reason = {invokations_missing, State#state.strict},
    {stop, Reason, Reason, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
no_expectations(I = {invokation, _M, _F, _A}, _From, State) ->
    case handle_stub_invokation(I, State#state.stub) of
        {ok, Answer} ->
            {reply, Answer, no_expectations, State};

        error ->
            Reason = {unexpected_invokation, {actual, I}},
            {stop, Reason, Reason, State}
    end;

no_expectations(verify,
         _From,
         State) ->
    {stop, normal, ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
terminate(_Reason, _StateName, State) ->
    unload_mock_modules(State).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_sync_event(_Evt, _From, _StateName, State) ->
    {stop, normal, ok, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_info(_Info, _StateName, State) ->
    {stop, normal, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_event(_Msg, _StateName, State) ->
    {stop, normal, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% api for generated mock code
%%

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
invoke(M, Mod, Fun, Args) ->
    case gen_fsm:sync_send_event(M, {invokation, Mod, Fun, Args}) of
        {return, Value} ->
            Value;
        {function, F} ->
            F(Args)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% internal functions
%%
%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unload_mock_modules(#state{mocked_modules = MMs}) ->
    [begin
	 code:delete(Mod),
	 code:purge(Mod),
         case MaybeBin of
              nothing ->
                 ignore;
             {just, {Mod, CoverCompiledBinary}} ->
                 code:load_binary(Mod, cover_compiled, CoverCompiledBinary)
         end
     end
     || {Mod, MaybeBin} <- MMs].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
install_mock_modules(#state{strict = ExpectationsStrict,
                            stub = ExpectationsStub,
			    blacklist = BlackList}) ->
    Expectations = ExpectationsStub ++ ExpectationsStrict,
    ModulesToMock = lists:usort([M || #expectation{m = M} <- Expectations] ++ BlackList),
    [install_mock_module(M, Expectations) || M <- ModulesToMock].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
install_mock_module(Mod, Expectations) ->
    MaybeBin = get_cover_compiled_binary(Mod),
    ModHeaderSyn = [erl_syntax:attribute(erl_syntax:atom(module),
					 [erl_syntax:atom(Mod)]),
                    erl_syntax:attribute(erl_syntax:atom(compile),
                                         [erl_syntax:list(
                                            [erl_syntax:atom(export_all)])])],
    Funs = lists:usort(
             [{F, length(A)} || #expectation{m = M, f = F, a = A} <- Expectations,
                                M == Mod]),
    FunFormsSyn = [mock_fun_syn(Mod, F, A) || {F, A} <- Funs],

    {ok, Mod, Code} =
        compile:forms([erl_syntax:revert(F)
                       || F <- ModHeaderSyn ++ FunFormsSyn]),

    code:delete(Mod),
    code:purge(Mod),
    {module, _} = load_module(Mod, Code),
    {Mod, MaybeBin}.


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
mock_fun_syn(Mod, F, Args) ->
    ArgsSyn = var_list_syn(Args),
    FunSyn = erl_syntax:atom(F),
    erl_syntax:function(
       FunSyn,
       [erl_syntax:clause(ArgsSyn,
                          none,
                          body_syn(Mod, FunSyn, ArgsSyn))]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
var_list_syn(Args) ->
    [erl_syntax:variable(list_to_atom("Arg_" ++ integer_to_list(I)))
     || I <- lists:seq(0, Args - 1)].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
body_syn(Mod, FunSyn, ArgsSyn) ->
    SelfStr = pid_to_list(self()),
    SelfSyn = erl_syntax:application(
                erl_syntax:atom(erlang),
                erl_syntax:atom(list_to_pid),
                [erl_syntax:string(SelfStr)]),
    [erl_syntax:application(
       erl_syntax:atom(?MODULE),
       erl_syntax:atom(invoke),
       [SelfSyn,
        erl_syntax:atom(Mod),
        FunSyn,
        erl_syntax:list(ArgsSyn)])].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_args(Args, ArgSpecs) ->
    try
        [begin
             if
                 is_function(E) ->
                     case E(A) of
                         true ->
                             ok;
                         _ ->
                             throw({error, I, E, A})
                     end;

                 A =/= E ->
                     throw({error, I, E, A});

                 A == E ->
                     ok
             end
         end
         || {I, A, E} <- lists:zip3(lists:seq(1, length(Args)),
                                    Args,
                                    ArgSpecs)] of
        _ ->
            true
    catch
        _:E ->
            E
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_stub_invokation({invokation, Mod, Fun, Args}, Stubs) ->
    case [MatchingStub
          || MatchingStub = #expectation {m = M, f = F, a = A} <- Stubs,
             M == Mod, F == Fun, length(Args) == length(A),
             check_args(Args, A) == true] of

        [#expectation{answer = Answer}|_] ->
            {ok, Answer};

        _ ->
            error
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec get_cover_compiled_binary(atom()) ->
                                       {just, term()} | nothing.
get_cover_compiled_binary(Mod) ->
    case code:which(Mod) of
        cover_compiled ->
            case ets:info(cover_binary_code_table) of
                undefined ->
                    nothing;
                _ ->
                    case ets:lookup(cover_binary_code_table, Mod) of
                        [Binary] ->
                            {just, Binary};
                        _ ->
                            nothing
                    end
            end;
        _ ->
            nothing
    end.
