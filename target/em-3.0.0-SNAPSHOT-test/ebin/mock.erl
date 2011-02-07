%%% This file was originally copied from Sven Heyll's blog:
%%%
%%% http://sheyll.blogspot.com/2009/02/erlang-mock-erlymock.html
%%%
%%% After that, we made some minor adaptations to it

%%%-------------------------------------------------------------------
%%% @author Sven Heyll
%%% @author Samuel Rivas <samuel.rivas@lambdastream.com>
%%% @doc A mocking library for erlang tests
%%%
%%% @end
%%% Created :  3 Dec 2009 by Samuel Rivas <samuel.rivas@lambdastream.com>
%%%-------------------------------------------------------------------
-module(mock).
-author('Sven Heyll').

-export([await/1,await/2,signal_fun/2,new/0, expect/7, expect/6, strict/6,
	 strict/5, o_o/5, stub/5, replay/1, verify/1, verify_after_last_call/1,
	 verify_after_last_call/2, invocation_event/1]).

-ifdef(MOCK_LOGGING_ON).
-define(LOG(F, D), error_logger:info_msg(F,D)).
-else.
-define(LOG(F,D), ok).
-endif.

%% use this to create a new instance of a mock process that is in programming
%% phase
new() ->
    Mock = spawn_link(fun() -> program_mock([],[],[]) end),
    ?LOG("mock ~w: created~n", [Mock]),
    Mock.

%% expect has the following options:
%% Orderchecking types: in_order, out_of_order, stub;
%% Answering: {return, ...}|{error, ...}|{throw, ...}|{exit, ...}
%%           |{rec_msg, Pid}|{function, Fun(Args) -> RetVal}
%%           |{function1, Fun(ArgList)}
expect(Mock, Type, Module, Function, Arguments, Answer = {AT, _})
  when is_list(Arguments), AT==return;
       AT==error;AT==throw;AT==exit;AT==rec_msg;AT==function;AT==function1 ->
    call(
      Mock,
      {expect, Type, Module, Function, length(Arguments), {Arguments, Answer}}).

%% this version of expect is suited for useing custom argument matchers
expect(Mock, Type, Module, Fun, Arity, MatcherFun, Answer)
  when is_integer(Arity), is_function(MatcherFun)->
    call(Mock, {expect, Type, Module, Fun, Arity, {custom, MatcherFun, Answer}}).

%% this is a short cut for expect(.., in_order, ..)
strict(Mock, M,F,Arity, Fun, Answer) when is_integer(Arity)->
    expect(Mock, in_order, M, F, Arity, Fun, Answer).

%% this is a short cut for expect(.., in_order, ..)
strict(Mock, M,F,Args, Answer) ->
    expect(Mock, in_order, M, F, Args, Answer).

%% this is a short cut for expect(.., out_of_order, ..)
o_o(Mock, M,F,Args, Answer) ->
    expect(Mock, out_of_order, M, F, Args, Answer).

%% this is a short cut for expect(.., stub, ..)
stub(Mock, M,F,Args, Answer) when is_list(Args)->
    expect(Mock, stub, M, F, Args, Answer);

%% this is a short cut for expect(.., stub, ..)
stub(Mock, M,F,Arity, Answer) when is_integer(Arity) ->
    expect(Mock, stub, M, F, Arity, fun(_) -> true end, Answer).

%% after the programming phase call this to switch to the replay phase
replay(Mock) ->
    call(Mock, replay).

%% after the verification phase use this to verify that all expected invocations
%% occured
%% XXX Note that this function has to be executed in the same process as replay
verify(Mock) ->
    verify_after_last_call(Mock, 0).

%% after the verification phase use this to verify that all expected invocations
%% occured
verify_after_last_call(Mock) ->
    verify_after_last_call(Mock, 1500).
verify_after_last_call(Mock, TimeOut) ->
    catch await(invocation_list_empty, TimeOut),
    try
	call(Mock, verify, 2000)
    catch
	{timeout, verify} ->
	    ?LOG("mock ~p: verify finished~n~n~n", [Mock]),
	    fail(mock_failed_before_verify)
    end,
    await(cleanup_finished),
    ?LOG("mock ~p: verify finished~n~n~n", [Mock]).


%%%-------------------------------------------------------------------
%%% utility functions for dealing with mock code called from a new process.
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% this will create an answering function that captures the current pid and
%% sends an atom to that pid, make sure to use await(atom()) to block until that
%% message is sent
%%--------------------------------------------------------------------
signal_fun(Atom, RetVal) ->
    SelfP = self(),
    {function1,
     fun(_) ->
	     signal(SelfP, Atom),
	     RetVal
     end}.

%%--------------------------------------------------------------------
%% internal signal function that send a message in "signal" protocol format to
%% some "await(...)"
%%--------------------------------------------------------------------
signal(Pid, Atom) ->
    ?LOG("signalling ~p from  ~p to ~p~n", [Atom, self(), Pid]),
    Pid ! {mock_signal, Atom}.

%%--------------------------------------------------------------------
%% this block the current process until signal(SameAtom) from another process is
%% invoked
%%--------------------------------------------------------------------
await(Atom) when is_atom(Atom) ->
    await(Atom, 2000).

await(Atom,To) when is_atom(Atom)->
    ?LOG("now awaiting ~p in process ~p~n", [Atom, self()]),
    receive
	{mock_signal, Atom} ->
	    ?LOG("await succeeded: ~p~n", [Atom])
    after To ->
	    fail({timeout, await, Atom})
    end.


fail(Pid, Reason) ->
    error_logger:error_msg("mock ~w: failed: ~w~n",[self(), Reason]),
    Pid ! {error, Reason}.

fail(Reason) ->
    error_logger:error_msg("mock ~w: failed: ~w~n",[self(), Reason]),
    throw({mock_failure, Reason}).

success(Pid) ->
    Pid ! {response, ok},
    success().

success() ->
    ?LOG("mock ~w: successfully finished.~n",[self()]),
    test_passed.

%% @private
%% @doc Call uses now a timeout, to avoid verify blocks when mock has
%% died. By now, we don't detect that mock blocks in any other call so
%% for the rest timeout is infinity.
%% @end
call(Name, Request) ->
    call(Name, Request, infinity).

call(Name, Request, Tout) ->
    Name ! {self(), Request},
    receive
	{error, Reason} ->
	    throw({mock_failure, Reason});
	{response, Response} ->
	    Response
    after Tout ->
	    throw({timeout, Request})
    end.

%% @private
%% @doc Returns a function to check arguments.
%% Doesn't need module, function or arity, why are they args?
%% @end
filter_fun(_, _, _, {Arguments, _}) ->
    fun(Args) ->
	    Args == Arguments
    end;

filter_fun(_, _, _, {custom, Matcher, _}) ->
    Matcher.

%% @private
%% @doc Returns the value that has to return the mock when is called
%% @end
answer_fun({_, Answer}) ->
    Answer;

answer_fun({custom, _, Answer}) ->
    Answer.

%% @private
%% @doc Headers of module abstract form, uses module and compile
%% @end
module_header_abstract_form(Mod) ->
    [{attribute,0,module,Mod},
     {attribute,0,compile,[export_all]}].

%% @private
%% @doc Abstract form of function definition, this function replaces calls to
%%      original module by calls to mock
%% @end
fundef_to_abstract_meta_form(Self, Mod, FunName, Arity) ->
    Line = 1,
    Params = [{var, Line, list_to_atom("A" ++ integer_to_list(I))}
	      || I <- seq(1,Arity)],
    {function, Line, FunName, Arity,
     [{clause, Line,
       Params, [],
       [{call, Line,
	 {remote, Line, {atom, Line, mock},
          {atom, Line, invocation_event}},
	 [{tuple, Line,
	   [{string,Line, Self},
	    {atom, Line, Mod}, {atom,Line, FunName}, {integer, Line, Arity},
	    lists:foldr(
	      fun(E,R) ->
		      {cons, Line, E, R}
	      end,
	      {nil, Line}, Params)]}]}]}]}.

%% @private
%% @doc Compile and load the abstract form made using
%%      module_header_abstract_form/1 and a list of functions from
%%      fundef_to_abstract_meta_form/4
%% @end
compile_and_load_abstract_form(AbsForm) ->
    CompRes = compile:forms(AbsForm),
    {ok, Mod, Code} = CompRes,
    code:purge(Mod),
    code:delete(Mod),
    {module, _} = load_module(Mod, Code).

%% @private
%% @doc Extracts the set of modules that are replaced by mock
%% @end
extract_module_set(Combined) ->
    sets:from_list(lists:map(fun({{M,_,_},_}) -> M end, Combined)).

%% @private
%% @doc Programs mock. Receives expect definitions to add to mock and
%% does replay.
%%
%% Expect definitions are divided in three lists, the definitions
%% using in_order, out_of_order and stub.
%% These definitions are a tuple: {{Mod, Fun, Arity},
%% {FilterFun, Answer}}
%% @end
program_mock(InOrder, OutOfOrder, Stub) ->
    receive
      {From, {expect, Type, Mod, Fun, Arity, Arg}} ->
	  FunDef = {{Mod, Fun, Arity},
		    {filter_fun(Mod, Fun, Arity, Arg), answer_fun(Arg)}},
	  From ! {response, ok},
	  case Type of
	    in_order ->
		program_mock([FunDef| InOrder], OutOfOrder, Stub);
	    out_of_order ->
		program_mock(InOrder, [FunDef| OutOfOrder], Stub);
	    stub ->
		program_mock(InOrder, OutOfOrder, [FunDef| Stub])
	  end;
      
      {From, replay} ->
	  program_mock_replay(InOrder, OutOfOrder, Stub, From);
      {From, What} ->
	  fail(From, {invalid_state, What})
    end.

%% @private
%% @doc Replay creates, compiles and loads modules to replace original modules
%% by stub. After that spawns a process to cleanup mock after finish
%% and send the signal 'cleanup_finished', that is expected to receive
%% in verify call. Calls record_invocations that waits for calls to
%% modules replaced by mock.
%% @end
program_mock_replay(InOrder, OutOfOrder, Stub, From) ->
    Self = pid_to_list(self()),
    Combined = InOrder ++ OutOfOrder ++ Stub,
    ModuleSet = extract_module_set(Combined),
    % Get binaries to restore after uninstall mock
    ModuleBins = get_binaries(ModuleSet),
    replace_modules_by_abstract_forms(Self, Combined, ModuleSet),
    From ! {response, ok},
    % spawn a cleanup process that will call the uninstall fun
    auto_cleanup(
      fun () ->
	      uninstall(ModuleBins),
	      signal(From, cleanup_finished)
      end, 
      self()),
    receive 
        {auto_cleanup_started, _ACPid} ->
            ok
    end,
    record_invocations(
      lists:reverse(InOrder),
      OutOfOrder,
      Stub,
      fun () ->
	      signal(From, invocation_list_empty)
      end).

%% @private
%% @doc Gets cover compiled binaries to restore after uninstall mock. 
%% 
%% @spec get_binaries(set()) -> [mod()]
%% mod() = atom() | {atom(), binary()}
%% @end
get_binaries(ModuleSet) ->
    Mods = sets:to_list(ModuleSet),
    lists:map(fun (Mod) ->
		      case code:which(Mod) of
			  cover_compiled ->
			      get_cover_compiled_binary(Mod);
			  _ ->
			      Mod
		      end
	      end, Mods).


%% @private
%% @doc Gets cover compiled binary to restore after uninstall mock. 
%% Reads `cover_binary_code_table', so it depends on cover implementation
%% 
%% @spec get_cover_compiled_binary(atom()) -> [mod()]
%% mod() = atom() | {atom(), binary()}
%% @end
get_cover_compiled_binary(Mod) ->
    case ets:info(cover_binary_code_table) of
	undefined ->
	    error_logger:error_msg(
	      "mock ~w: ERROR table of cover compiled binaries not found~n",
	      [self()]),
	    Mod;
	_ ->
	    case ets:lookup(cover_binary_code_table, Mod) of
		[] ->
		    error_logger:error_msg(
		      "mock ~w: ERROR cover compiled binary of ~p not found~n",
		      [self(), Mod]),
		    Mod;
		[H | _] ->
		    H
	    end
    end.

%% @private
%% @doc Creates, compiles and loads modules to replace original modules by stub.
%% @end
replace_modules_by_abstract_forms(Self, Combined, ModuleSet) ->
    sets:fold(
      fun (Mod, _) ->
	      FunsOfModSet = sets:from_list(
			       lists:foldl(
				 fun ({{M, F, A}, _}, Acc) ->
					 if Mod == M -> [{F, A}| Acc];
					    true -> Acc
					 end
				 end, [], Combined)),
	      HeaderForm = module_header_abstract_form(Mod),
	      FunctionForms = sets:fold(
				fun ({F, A}, FFAcc) ->
					[fundef_to_abstract_meta_form(
					   Self, Mod, F, A)| FFAcc]
				end,
				[],
				FunsOfModSet),
	      _CLRes = compile_and_load_abstract_form(
			HeaderForm ++ FunctionForms),
	      ?LOG(
		"mock ~w: created and loaded mock code ~w~n",
		[self(), _CLRes])
      end, [], ModuleSet).

%% @private
%% @doc Waits for calls to modules replaced by mock. Performs them
%% and throws exceptions in case of unexpected invocations. Function
%% definition in each list of waited calls is an improper list.
%%
%% Sends 'invocation_list_empty' event when InOrder and OutOfOrder are
%% empty, after that recursive calls are done over record_invocations/1
%%
%% @spec record_invocations(InOrder::[fundef()], OutOfOrder::[fundef()],
%%              Stub::[fundef()], function() | undefined) -> test_passed
%%  fundef() = {{Mod::atom(), Fun::atom(), Arity::integer()} ,
%%             {FilterFun::function(), answer()}}
%%  answer() = {return, term()}|{error, term()}|{throw, term()}|{exit, term()}
%%           |{rec_msg, Pid}|{function, function()}
%%           |{function1, function()}
%% @end
record_invocations([], [], Stub, EF) ->
    EF(),
    record_invocations(Stub);
record_invocations(InOrder, OutOfOrder, Stub, EF) ->
    % wait for all incoming invocations, expect every invocation and crash if
    % the invocation was not correct
    receive
	Invocation = {_, _, _, _, _} ->
	    expect_invocation(InOrder, OutOfOrder, Stub, EF, Invocation);      
	{From, verify} -> verify_invocation(InOrder, OutOfOrder, From);	
	{From, What} ->
	    fail(From, {invalid_state, What})
    end.

%% @private
%% @doc Waits for calls to modules replaced by mock with only stub calls.
%% @end 
record_invocations(Stub) ->
    receive
	Invocation = {ProcUnderTestPid, Mod, Fun, Arity, Args} ->
	    InvMatcher = invocation_matcher(Mod, Fun, Arity, Args), 
	    case lists:filter(InvMatcher, Stub) of
		[StubDef| _] ->
		    {_, {_, Function}} = StubDef,
		    ProcUnderTestPid ! {mock_process_gaurd__, Function},
		    record_invocations(Stub);
		_ ->
		    unexpected_invocation(Invocation, ProcUnderTestPid)
	    end;
	{From, verify} -> success(From);
	{From, What} -> fail(From, {invalid_state, What})
    end.

%% @private
%% @doc Manages expect invocation. Checks in_order, out_of_order and
%% stub invocations, sends invocation_list_empty signal when in_order
%% and out_of_order lists are empty.
%% @end
expect_invocation(InOrder, OutOfOrder, Stub, EF,
		  Invocation = {ProcUnderTestPid, Mod, Fun, Arity, Args}) ->
    InvMatcher = invocation_matcher(Mod, Fun, Arity, Args),
    try
      case is_in_order_invocation(InOrder, InvMatcher) of    
	  true ->
	      in_order_invocation(InOrder, OutOfOrder, Stub, EF,
				  ProcUnderTestPid);
	  false ->
	      case lists:partition(InvMatcher, OutOfOrder) of
		  {[OOODef| Rest1], Rest2} ->
		      out_of_order_invocation(InOrder, Stub, EF,
					      ProcUnderTestPid,
					      OOODef, Rest1, Rest2);
		  {[], _} ->
		      case lists:filter(InvMatcher, Stub) of
			  [StubDef| _] ->
			      stub_invocation(InOrder, OutOfOrder, Stub, EF,
					      ProcUnderTestPid, StubDef);
			  _ ->
			      unexpected_invocation(Invocation,
						    ProcUnderTestPid)
		      end
	      end
      end
    catch
	{mock_failure, _} = What ->
	    throw(What);
	ET:EX ->
	    matching_function_error(Invocation, ProcUnderTestPid, ET, EX)
    end.

is_in_order_invocation([], _InvMatcher) ->
    false;
is_in_order_invocation([Test | _], InvMatcher) ->
    InvMatcher(Test).
    
%% @private
%% @doc Error in matching function. This makes mock fail
%% @end
matching_function_error(Invocation, ProcUnderTestPid, ET, EX) ->
    Reason = {matching_function_is_incorrect,
	      Invocation, {ET, EX}},
    ProcUnderTestPid ! {mock_process_gaurd__, {error, Reason}},    
    fail(Reason).

%% @private
%% @doc Returns invocation function matcher, to check if call is part
%% of the invocation list. 
%% @end
invocation_matcher(Mod, Fun, Arity, Args) ->
    fun ({{M, F, A}, {Pred, _}}) ->
	    {M, F, A} == {Mod, Fun, Arity} andalso Pred(Args)
    end.

%% @private
%% @doc Unexpected invocation. This makes mock fail
%% @end
unexpected_invocation(Invocation, ProcUnderTestPid) ->
    Reason = {unexpected_invocation, Invocation},
    ProcUnderTestPid ! {mock_process_gaurd__,
			{error, Reason}},
    fail(Reason).

%% @private
%% @doc This is part of verify behaviour, checks that there isn't more
%% in_order or out_of_order calls. 
%% @end
verify_invocation(InOrder, OutOfOrder, From) ->
    case {InOrder, OutOfOrder} of
      {[], []} ->
	  success(From);
      MissingRest ->
	  fail(From, {expected_invocations_missing, MissingRest})
    end.

%% @private
%% @doc Expected invocation to stub call. Replies to module call and
%% continues mock execution
%% @end
stub_invocation(InOrder, OutOfOrder, Stub, EF,
		ProcUnderTestPid, StubDef) ->
    {_, {_, Function}} = StubDef,
    ProcUnderTestPid ! {mock_process_gaurd__, Function},
    record_invocations(InOrder, OutOfOrder, Stub, EF).

%% @private
%% @doc Expected invocation to out_of_order call. Replies to module call and
%% continues mock execution
%% @end
out_of_order_invocation(InOrder, Stub, EF, ProcUnderTestPid,
			OOODef, Rest1, Rest2) ->
    {_, {_, Function}} = OOODef,
    ProcUnderTestPid ! {mock_process_gaurd__, Function},
    record_invocations(InOrder, Rest1 ++ Rest2, Stub, EF).

%% @private
%% @doc Expected invocation to in_order call. Replies to module call and
%% continues mock execution
%% @end
in_order_invocation(InOrder, OutOfOrder, Stub, EF,
		    ProcUnderTestPid) ->
    [{_, {_, Function}}| IOR] = InOrder,
    ProcUnderTestPid ! {mock_process_gaurd__, Function},
    record_invocations(IOR, OutOfOrder, Stub, EF).

invocation_event({MockPidStr, Mod, Fun, Arity, Args}) ->
    MockPid = list_to_pid(MockPidStr),
    ?LOG(
      "mock ~w: invocation: ~w:~w/~w ~w~n",[MockPid, Mod, Fun, Arity, Args]),
    MockPid ! {self(), Mod, Fun, Arity, Args},
    receive
	{mock_process_gaurd__, {return, Answer}} ->
	    Answer;
	{mock_process_gaurd__, {error, E}} ->
	    erlang:error(E);
	{mock_process_gaurd__, {throw, E}} ->
	    throw(E);
	{mock_process_gaurd__, {exit, R}} ->
	    exit(R);
	{mock_process_gaurd__, {function, F}} ->
	    ?LOG("mock ~w: invoking answerer~n",[MockPid]),
	    R = apply(F,Args),
	    ?LOG(
	      "mock ~w: answerer returned: ~w~n",[MockPid,R]),
	    R;
	{mock_process_gaurd__, {function1, F}} ->
	    ?LOG("mock ~w: invoking answerer~n",[MockPid]),
	    R = F(Args),
	    ?LOG(
	      "mock ~w: answerer returned: ~w~n",[MockPid,R]),
	    R;
	{mock_process_gaurd__, {rec_msg, P}} ->
	    ?LOG(
	      "mock ~w: receiving message for ~w~n",[MockPid,P]),
	    _Msg = receive
		      M ->
			  P ! M
		  end,
	    ?LOG(
	      "mock ~w: message ~w delivered to ~w~n",[MockPid,_Msg,P])
    end.

seq(A, E) when A > E -> [];
seq(A, E) -> lists:seq(A,E).

%% @private
%% @doc Delete and purge mock modules. Load cover compiled modules.
%%
%% @spec uninstall([atom()]) -> any()
%% @end
uninstall(ModuleList) ->
    lists:map(
      fun ({Mod, Bin}) ->
	      uninstall_module(Mod),
	      code:load_binary(Mod, cover_compiled, Bin);
	  (Mod) ->
	      uninstall_module(Mod)
      end, ModuleList).

uninstall_module(Mod) ->
    ?LOG("Deleting and purging module ~p~n", [Mod]),
    code:purge(Mod),
    code:delete(Mod).

auto_cleanup(CleanupFun, FromPid) ->
    spawn_link(
      fun() ->              
	      erlang:process_flag(trap_exit, true),
	      ?LOG(
		"auto cleanup handler ~p waiting for the end of ~p ...~n", 
                [self(), FromPid]),
              FromPid ! {auto_cleanup_started, self()},
	      receive
		  _Msg = {'EXIT', _From, _Reason} ->
		      ?LOG(
			"auto cleanup handler ~p receive exit message ~p.~n",
			[self(), _Msg]),
		      CleanupFun();
		  _Ather ->
		      error_logger:warning_msg(
			"auto cleanup handler ~p received "
			"unexpected message  ~p.~n",
			[self(), _Ather])
	      end
      end).
