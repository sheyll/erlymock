%%%-----------------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2011-2017 Sven Heyll
%%% @doc
%%% The module name 'em' stands for 'Erly Mock'.
%%%
%%% <p>This mocking library works similar to Easymock.</p>
%%%
%%% <p>After a mock process is started by {@link new/0} it can be programmed to
%%% expect function calls and to react to them in two ways: <ul><li>by returning
%%% a value</li><li>by executing an arbitrary function</li></ul> This is done
%%% with {@link strict/4}, {@link strict/5}, {@link stub/4}, {@link stub/5} </p>
%%%
%%% <p>Before the code under test is executed, the mock must be told
%%% that the programming phase is over by {@link replay/1}.</p>
%%%
%%% <p>In the next phase the code under test is run, and might or might not call
%%% the functions mocked.  The mock process checks that all functions programmed
%%% with {@link strict/4}, {@link strict/5} are called in the correct order,
%%% with the expected arguments and reacts in the way defined during the
%%% programming phase. If a mocked function is called although another function
%%% was expected, or if an expected function was called with different
%%% arguments, the mock process dies and prints a comprehensive error message
%%% before failing the test.</p>
%%%
%%% <p>To support mock invokations from multiple processes the strictness
%%% requirement can be reduced to calls belonging to the same group. {@link
%%% new_groups/2} creates a list of named groups, where calls belongig to
%%% different groups may occur in any order. A group is passed as mock reference
%%% (1st parameter) to {@link strict/5} or {@link strict/4}. Use {@link
%%% await/1} with a list of groups to block the caller until all groups
%%% are finished, i.e. the expectations assigned to each group via {@link
%%% strict/5} were invoked. NOTE: It is prohibited to use the same expectations
%%% with different return values among a list groups created together.</p>
%%%
%%% <p>At the end of a unit test {@link await_expectations/1} is called to
%%% await all invocations defined during the programming phase.</p>
%%%
%%% <p>An alternative to {@link await_expectations/1} is {@link verify/1}. It is
%%% called to check for missing invocations at the end of the programming phase,
%%% if any expected invocations are missing at verify will throw an
%%% exception.</p>
%%%
%%% <p>When the mock process exits it tries hard to remove all modules, that
%%% were dynamically created and loaded during the programming phase.</p>
%%%
%%% NOTE: This library works by purging the modules mocked and replacing them
%%% with dynamically created and compiled code, so be careful what you mock,
%%% i.e. it brings chaos to mock modules from kernel. This also implies, that
%%% tests that mock the same modules must be run sequentially.
%%%
%%% Apart from that, it is very advisable to <b>only mock owned modules</b>
%%% anyway.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2011-2017 Sven Heyll
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to
%%% deal in the Software without restriction, including without limitation the
%%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%%% sell copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%%% IN THE SOFTWARE.
%%%
%%%-----------------------------------------------------------------------------

-module(em).

-behaviour(gen_statem).

%% public API ---
-export([new/0,
         new/1,
         new_groups/2,
         nothing/2,
         strict/4,
         strict/5,
         any/0,
         zelf/0,
         stub/4,
         stub/5,
         replay/1,
         replay/2,
         await/2,
         await_expectations/1,
         verify/1,
         call_log/1]).

%% gen_statem callbacks ---
-export([programming/3,
         replaying/3,
         no_expectations/3,
         deranged/3,
         callback_mode/0,
         init/1,
         terminate/3,
         code_change/4
         ]).

%% !!!NEVER CALL THIS FUNCTION!!! ---
-export([invoke/4]).

-export_type([group/0, group_tag/0, timeout_millis/0, load_mode/0]).

-include_lib("em/include/em.hrl").

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

%%------------------------------------------------------------------------------
%% Determine if loading of modules happens immediately and fails if another
%% test loaded the modules, or if the process will wait until the modules can
%% be loaded.
%%------------------------------------------------------------------------------
-type load_mode() :: wait_for_modules | nonblocking.

%%------------------------------------------------------------------------------
%% A group is a pair with a tag for a group and a mock process.
%%------------------------------------------------------------------------------
-type group_tag() :: {term(), reference()}.
-type group() :: {group, pid(), group_tag()}.

%%------------------------------------------------------------------------------
%% Timout for {@link replay/2}
%%------------------------------------------------------------------------------
-type timeout_millis() :: non_neg_integer() | infinity.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% API
%%

%%------------------------------------------------------------------------------
%% @doc
%% Spawn a linked mock process and return the pid. <p>This is usually the
%% first thing to do in each unit test. The resulting pid is used in the other
%% functions below.</p> <p>NOTE: only a single mock proccess is required for a
%% single unit test case. One mock process can mock an arbitrary number of
%% different modules.</p> <p>When the mock process dies, all uploaded modules
%% are purged from the code server, and all cover compiled modules are
%% restored.</p> <p>When the process that started the mock exits, the mock
%% automatically cleans up and exits.</p> <p>After new() the mock is in
%% 'programming' state.</p>
%%
%% This is equal to `new(wait_for_modules)'.
%%
%% @see new/1
%%
%% @end
%%------------------------------------------------------------------------------
-spec new() ->
                 group().
new() ->
    new(wait_for_modules).

%%------------------------------------------------------------------------------
%% @doc
%% Like `new/0' but allow control over how the mock-module loading is done.
%%
%% `wait_for_modules' causes `replay/1' to block until no other process uses
%% any of the modules required by this mock.
%% `nonblocking' causes `replay/1' to try to claim all of the modules needed
%% immediately, and fails with `{error, Reason}' if a module is currently mocked
%% by another process.
%%
%% @see new/0
%% @since 7.1.0
%% @end
%%------------------------------------------------------------------------------
-spec new(load_mode()) ->
                 group().
new(Mode) ->
    {ok, M} = gen_statem:start_link(?MODULE, {erlang:self(), Mode}, []),
    RootTag = {root, make_ref()},
    {group, M, RootTag}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
callback_mode() -> state_functions.

%%------------------------------------------------------------------------------
%% @doc
%% Create a group handle to assign mock expectation to. The result can be passed
%% to {@link strict/4} or {@link strict/5} and {@link await/2}.
%% @end
%%------------------------------------------------------------------------------
-spec new_groups(group(), [term()]) ->
                       [group()].
new_groups({group, M, _}, GroupNames) ->
    GroupCluster = make_ref(),
    [{group,
      M,
      {GroupName, GroupCluster}}
     || GroupName <- GroupNames].

%%------------------------------------------------------------------------------
%% @doc
%% Add an expectation during the programming phase for a specific function
%% invokation.
%%
%% <p>All expectations defined by 'strict' define an order in which the
%% application must call the mocked functions, hence the name 'strict' as oposed
%% to 'stub' (see below).</p>
%%
%% <p>The parameters are:
%% <ul>
%% <li><code>M</code> the mock pid, returned by {@link new/0}</li>
%% <li><code>Mod</code> the module of the function to mock</li>
%% <li><code>Fun</code> the name of the function to mock</li>
%% <li><code>Args</code> a list of expected arguments.
%% Each list element is either a value that will be matched to the actual value
%% of the parameter at that position, or a predicate function which will be
%% applied to the actual argument.</li>
%% </ul></p>
%%
%% <p>This function returns a reference that identifies the expectation. This
%% reference can be passed to {@link await/2} which blocks until the expected
%% invokation happens.</p>
%%
%% <p> The return value, that the application will get when calling the mocked
%% function in the replay phase is simply the atom <code>ok</code>. This
%% differentiates this function from {@link strict/5}, which allows the
%% definition of a custom response function or a custom return value.  </p>
%%
%% NOTE: This function may only be called between <code>new/0</code> and {@link
%% replay/1} - that is during the programming phase.
%%
%% @end
%%------------------------------------------------------------------------------
-spec strict(group(), atom(), atom(), args()) ->
                    reference().
strict(M, Mod, Fun, Args) ->
    strict(M, Mod, Fun, Args, {return, ok}).

%%------------------------------------------------------------------------------
%% @doc
%% This function behaves like {@link strict/4}
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
%% @end
%%------------------------------------------------------------------------------
-spec strict(group(), atom(), atom(), args(), answer()) ->
                    reference().
strict({group, M, Group}, Mod, Fun, Args, Answer = {return, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    gen_statem:call(M, {strict, Group, Mod, Fun, Args, Answer}, infinity);

strict({group, M, Group}, Mod, Fun, Args, Answer = {function, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    gen_statem:call(M, {strict, Group, Mod, Fun, Args, Answer}, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Defines a what happens when a function is called whithout recording any
%% expectations. The invocations defined by this function may happen in any order
%% any number of times. The way, the invocation is defined is analog to
%% @see strict/4. <code>strict/4</code>
%% @end
%%------------------------------------------------------------------------------
-spec stub(group(), atom(), atom(), args()) ->
                  ok.
stub(M, Mod, Fun, Args) ->
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
-spec stub(group(), atom(), atom(), args(), answer()) ->
                  ok.
stub({group, M, Group = {root, _}}, Mod, Fun, Args, Answer = {return, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_statem:call(M, {stub, Group, Mod, Fun, Args, Answer}, infinity);

stub({group, M, Group = {root, _}}, Mod, Fun, Args, Answer = {function, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_statem:call(M, {stub, Group, Mod, Fun, Args, Answer}, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% This is used to express the expectation that no function of a certain module
%% is called. This will cause each function call on a module to throw an 'undef'
%% exception.
%% @end
%%------------------------------------------------------------------------------
-spec nothing(group(), atom()) ->
		     ok.
nothing({group, M, {root, _}}, Mod) when is_pid(M), is_atom(Mod) ->
   ok = gen_statem:call(M, {nothing, Mod}, infinity).

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
-spec replay(group()) -> ok.
replay(G) ->
    replay(G, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Finishes the programming phase and switches to the replay phase, expecting
%% that invokations are recorded at least once every `InvokationTimeout' millis.
%% @see replay/1
%% @end
%%------------------------------------------------------------------------------
-spec replay(group(), timeout_millis()) -> ok.
replay({group, M, {root, _}}, InvokationTimeout) ->
    ok = gen_statem:call(M, {replay, InvokationTimeout}, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Block until a specific invokation defined via {@link strict/4} during the
%% programming phase was made. <p>The handle for the specific invokation is the
%% value returned by {@link strict/4}.</p> <p>The return value contains the
%% parameters and the pid of the recorded invokation. This function maybe called
%% anytime before or after the referenced invokation has actually
%% happened.</p><p>If the handle is not valid, an error is returned.</p>
%% @end
%% ------------------------------------------------------------------------------
-spec await(group(), reference()) ->
                   {success,
                    InvPid :: pid(),
                    Args :: [term()]} |
                   {error, term()}.
await({group, M, {root, _}}, Handle) ->
    gen_statem:call(M, {await, Handle}, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Retrieve a list of successfully mocked invokations, i.e. all calls that were
%% accepted by the `em' process in the `replay' phase. Both strict and stub
%% invokations are recorded.  NOTE: The Answer might as well be a function,
%% depending on the `return' argument passed to `strict' or `stub'.
%% @end
%%------------------------------------------------------------------------------
-spec call_log(group()) ->  [{Mod :: atom(),
                              Func :: atom(),
                              Args :: [term()],
                              Answer :: term()}].
call_log({group, M, {root, _}}) ->
    gen_statem:call(M, get_call_log, infinity).

%%------------------------------------------------------------------------------
%% @doc
%% Wait until all invokations defined during the programming phase were made.
%% After this functions returns, the mock can be expected to exit and clean up
%% all modules installed.
%% @end
%%------------------------------------------------------------------------------
-spec await_expectations(group()) -> ok.
await_expectations({group, M, {root, _}}) ->
    case
        gen_statem:call(M, await_expectations, infinity)
    of
        ok ->
            ok;

        Error ->
            error_logger:error_msg("erlymock verification failed: ~p",
                                   [Error]),
            error(Error)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Finishes the replay phase. If the code under test did not cause all expected
%% invokations defined by {@link strict/4} or {@link strict/5}, the
%% call will fail with <code>badmatch</code> with a comprehensive error message.
%% Otherwise the mock process exits normally, returning <code>ok</code>.
%% @end
%%------------------------------------------------------------------------------
-spec verify(group()) -> ok.
verify({group, M, {root, _}}) ->
    case
        gen_statem:call(M, verify, infinity)
    of
        ok ->
            ok;

        Error ->
            error_logger:error_msg("erlymock verification failed: ~p",
                                   [Error]),
            error(Error)
    end.

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

%%------------------------------------------------------------------------------
%% @doc
%% Utility function that can be used as a match function in an
%% argument list to match <code>self()</code>, e.g. when it matches the pid of the
%% process, that calls the funtion during the replay phase.
%% @end
%%------------------------------------------------------------------------------
-spec zelf() ->
                  atom().
zelf() ->
    '$$em zelf$$'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% internal state
%%

-record(expectation,
        {id               :: reference(),
         g                :: group_tag(),
         m                :: atom(),
         f                :: atom(),
         a                :: args(),
         answer           :: answer(),
         listeners = []   :: [GenFsmFrom :: term()]}).

-record(strict_log,
        {grpt             :: group_tag(),
         eref             :: reference(),
         ipid             :: pid(),
         args             :: [term()]}).

-record(state,
        {test_proc        :: pid(),
         inv_to           :: timeout_millis() | infinity,
         strict           :: [#expectation{}],
         strict_log       :: [#strict_log{}],
         stub             :: [#expectation{}],
         call_log         :: [{Mod :: atom(),
					          Func :: atom(),
						      Args :: [term()],
						      Answer :: term()}],
         blacklist        :: [atom()],
         mocked_modules   :: [{atom(), {just, term()}|nothing}],
         on_finished      :: term(), % GenFsmFrom
         error = no_error :: no_error | term(),
         mode = wait_for_modules :: load_mode()
        }).

-type statedata() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% gen_statem callbacks
%%

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec init({TestProc :: term(), load_mode()}) ->
                  {ok, atom(), StateData :: statedata()}.
init({TestProc, LoadMode}) ->
    process_flag(sensitive, true),
    process_flag(trap_exit, true),
    erlang:trace(self(), false, [all]),
    {ok,
     programming,
     #state{
        test_proc = TestProc,
        inv_to = infinity,
        strict = [],
        strict_log = [],
        stub = [],
        call_log =[],
        blacklist = [],
        mocked_modules = [],
        mode = LoadMode}}.


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec programming(gen_statem:event_type(), EventContent :: term(), statedata()) ->
                         gen_statem:event_handler_result(no_expectations|replaying).
programming({call, From},
            {strict, Group, Mod, Fun, Args, Answer},
            State = #state{strict = Strict}) ->
    InvRef = make_ref(),
    {keep_state,
     State#state{
       strict = [#expectation{id = InvRef,
                              g = Group,
                              m = Mod,
                              f = Fun,
                              a = Args,
                              answer = Answer,
                              listeners = []}
                 |Strict]},
       {reply, From, InvRef}};

programming({call, From},
            {stub, Group, Mod, Fun, Args, Answer},
            State = #state{stub = Stub}) ->
    InvRef = make_ref(),
    {keep_state,
     State#state{
       stub = [#expectation{id = InvRef,
                            g = Group,
                            m = Mod,
                            f = Fun,
                            a = Args,
                            answer = Answer,
                            listeners = []}
              |Stub]},
     {reply, From, ok}};

programming({call, From},
            {nothing, Mod},
            State = #state{blacklist = BL}) ->
    {keep_state,
     State#state{
       blacklist = [Mod | BL]},
     {reply, From, ok}};

programming({call, From},
            {replay, InvTo},
            State) ->
    NextState = load_mock_modules(
                  prepare_strict_invocations(
                      set_invokation_timeout(InvTo, State))),
    NextStateName = case NextState#state.strict of
                        [] -> no_expectations;
                        _ -> replaying
                    end,
    {next_state,
     NextStateName,
     NextState,
     [{reply, From, ok}
     |[start_invokation_timer(NextState)||NextStateName == replaying]]};

programming({call, From}, get_call_log, State) ->
    {keep_state_and_data,
     {reply, From, lists:reverse(State#state.call_log)}};

programming({call, From}, Event, _State) ->
    {keep_state_and_data,
     {reply, From, {error, {bad_request, programming, Event}}}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec replaying(gen_statem:event_type(), EventContent :: term(), statedata()) ->
                       gen_statem:event_handler_result(no_expectations|deranged).
replaying({timeout, invokation_timeout},
          invokation_timeout,
          State = #state{ strict = Expectations}) ->
    {stop, {invokation_timeout, {missing_invokations, Expectations}}, State};

replaying({call, From},
          Inv = {invokation, _M, _F, _A, _IPid},
          St) ->
    case
        find_matching_expectation(
          Inv,
          [],
          get_next_expectations(St))
    of
        {ok, E = #expectation{}} ->
            stop_or_continue_replay(
              answer_invokation(Inv, E, From),
              remove_expectation(
                E,
                log_invokation(Inv, E, St)));

        {error, Error} ->
            enter_deranged([{reply, From, {'$em_error', Error}}], Error, St)
    end;

replaying({call, From}, verify, State) ->
    Reason = {invokations_missing, State#state.strict},
    {stop_and_reply, normal, {reply, From, Reason}, State};

replaying({call, From}, {await, H}, State) ->
    {NewState, ReplyActions} = add_invokation_listener(From, H, State),
    {keep_state, NewState, ReplyActions};

replaying({call, From},
          await_expectations,
          State = #state{on_finished = undefined}) ->
    {keep_state, State#state{ on_finished = From }};

replaying({call, From}, get_call_log, State) ->
    {keep_state_and_data,
     {reply, From, lists:reverse(State#state.call_log)}};

replaying({call, From}, Event, _) ->
    {keep_state_and_data, {reply, From, {error, {bad_request, replaying, Event}}}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec no_expectations(gen_statem:event_type(),
                      EventData :: term(),
                      statedata()) ->
                        gen_statem:event_handler_result(deranged).
no_expectations({call, From},
                {invokation, Mod, Fun, Args, IPid},
                State = #state{call_log = CallLog}) ->
    Stubs = State#state.stub,
    MatchingStubs = [Stub
                    || Stub = #expectation {m = M, f = F, a = A} <- Stubs,
                       M == Mod,
                       F == Fun,
                       length(Args) == length(A),
                       check_args(Args, A, IPid)],
    case MatchingStubs of
        [#expectation{answer = Answer}|_] ->
            {keep_state,
             State#state{call_log = [{Mod, Fun, Args, Answer}|CallLog]},
             [{reply, From, Answer}]};

        _ ->
            Error = {unexpected_invokation, {invokation, Mod, Fun, Args, IPid}},
            enter_deranged([{reply, From, {'$em_error', Error}}], Error, State)
    end;

no_expectations({call, From}, verify, State) ->
    {stop_and_reply, normal, {reply, From, ok}, State};

no_expectations({call, From}, await_expectations, State) ->
    {stop_and_reply, normal, {reply, From, ok}, State};

no_expectations({call, From}, {await, H}, State) ->
    {NewState, ReplyActions} = add_invokation_listener(From, H, State),
    {keep_state, NewState, ReplyActions};

no_expectations({call, From}, get_call_log, State) ->
    {keep_state_and_data,
     {reply, From, lists:reverse(State#state.call_log)}};

no_expectations({call, From}, Event, _) ->
    {keep_state_and_data,
     {reply, From, {error, {bad_request, no_expectations, Event}}}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec deranged(gen_statem:event_type(),
               EventData :: term(),
               statedata()) ->
                    gen_statem:state_callback_result(gen_statem:reply_action()).

deranged({call, From}, verify, State = #state{ error = Error }) ->
    {stop_and_reply, normal, {reply, From, Error}, State};

deranged({call, From}, await_expectations, State = #state{ error = Error }) ->
    {stop_and_reply, normal, {reply, From, Error}, State};

deranged({call, From}, {await, _}, _) ->
    {keep_state_and_data, {reply, From, {error, mock_deranged}}};

deranged({call, From}, {invokation, _M, _F, _A, _IPid}, _) ->
    {keep_state_and_data, {reply, From, {'$em_error', mock_deranged}}};

deranged({call, From}, get_call_log, State) ->
    {keep_state_and_data,
     {reply, From, lists:reverse(State#state.call_log)}};

deranged({call, From}, Event, _) ->
    {keep_state_and_data,
     {reply, From, {error, {bad_request, deranged, Event}}}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec terminate(Reason :: term(), StateName :: atom(),
                StateData :: statedata()) -> no_return().
terminate(_Reason, _StateName, State) ->
    try unload_mock_modules(State) catch _:_ -> ok end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec code_change(OldVsn :: term(), StateName :: atom(), State :: statedata(),
                  Extra :: term()) ->
                         {ok, NextState :: atom(), NewStateData :: statedata()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% api for generated mock code
%%

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec invoke(M :: term(), Mod :: term(), Fun :: fun(), Args :: list()) ->
                    {Value :: term()}.
invoke(M, Mod, Fun, Args) ->
    Trace = erlang:get_stacktrace(),
    try io:format("~nEM: ~w:~w ~p",[Mod,Fun, Args]) catch _:_ -> ok end,
    Rv = case gen_statem:call(M, {invokation, Mod, Fun, Args, self()}, infinity) of
             {return, Value} ->
                 Value;
             {'$em_error' , WTF} ->
                 (catch io:format(" *ERROR* ->  ~p~nAT: ~p~n~n",[WTF, Trace])),
                 exit({mock_error, WTF});
             {function, F} ->
                 F(Args)
         end,
    try io:format(" ->  ~p~n",[Rv]) catch _:_ -> ok end,
    Rv.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% internal functions
%%
%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unload_mock_modules(#state{mocked_modules = MMs}) ->
    [begin
         really_delete(Mod),
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
load_mock_modules(State = #state{ strict    = ExpectationsStrict,
                                  stub      = ExpectationsStub,
                                  blacklist = BlackList,
                                  mode      = Mode}) ->
    Expectations = ExpectationsStub ++ ExpectationsStrict,
    ExpectationModules = lists:usort([M || #expectation{m = M} <- Expectations]),
    ModulesToMock = lists:usort(ExpectationModules ++ BlackList),
    case Mode of
        wait_for_modules ->
            ok = em_mocking_queue:enqueue_and_wait(erlang:self(), ModulesToMock);
        nonblocking ->
            ok = em_mocking_queue:lock_immediately(erlang:self(), ExpectationModules)
    end,
    MockedModules =
        [begin
             ModExpectations = [E || E=#expectation{m = Me} <- Expectations,
                                     M =:= Me],
             assert_not_mocked(M),
             case load_original_module(M) of
                 existing_module ->
                     [assert_mocked_function_exists(E)
                      || E <- ModExpectations];
                 fantasy_module ->
                     ok
             end,
             load_mock_module(M, ModExpectations)
         end || M <- ExpectationModules],
    State#state{ mocked_modules = MockedModules }.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
assert_not_mocked(Mod) ->
    try Mod:module_info(attributes) of
        Attrs ->
            case lists:keyfind(?ERLYMOCK_COMPILED, 1 , Attrs) of
                false ->
                    ok;
                _ ->
                    throw({em_error_module_already_mocked, Mod})
            end
    catch
        _:_ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
load_mock_module(Mod, Expectations) ->
    MaybeBin = get_cover_compiled_binary(Mod),
    ModHeaderSyn = [erl_syntax:attribute(erl_syntax:atom(module),
					 [erl_syntax:atom(Mod)]),
                    erl_syntax:attribute(erl_syntax:atom(?ERLYMOCK_COMPILED),
					 [erl_syntax:atom(true)]),
                    erl_syntax:attribute(erl_syntax:atom(compile),
                                         [erl_syntax:list(
                                            [erl_syntax:atom(export_all)])])],
    Funs = lists:usort(
             [{F, length(A)} ||
                 #expectation{ m = M, f = F, a = A } <- Expectations,
                 M == Mod]),
    FunFormsSyn = [mock_fun_syn(Mod, F, A) || {F, A} <- Funs],

    {ok, Mod, Code} =
        compile:forms([erl_syntax:revert(F)
                       || F <- ModHeaderSyn ++ FunFormsSyn]),
    really_delete(Mod),
    FName = lists:flatten(io_lib:format("~w.beam", [Mod])),
    {module, _} = code:load_binary(Mod, FName, Code),
    {Mod, MaybeBin}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
really_delete(Mod) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod).

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
    SelfStr = pid_to_list(erlang:self()),
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
prepare_strict_invocations(S = #state{ strict = Strict }) ->
    S#state{ strict = lists:reverse(Strict) }.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_args(Args, ArgSpecs, InvokationPid) ->
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
                 true ->
                     case E of

                         '$$em zelf$$' ->
                             if A =/= InvokationPid ->
                                     throw({error, I, E, A});
                                true ->
                                     ok
                             end;

                         A ->
                             ok;

                         _Otherwise ->
                             throw({error, I, E, A})
                     end
             end
         end
         || {I, A, E} <- lists:zip3(lists:seq(1, length(Args)),
                                    Args,
                                    ArgSpecs)]
    of
        _ -> true
    catch
        E -> E
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

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec add_invokation_listener(gen_statem:from(), Ref :: term(), statedata()) ->
    {statedata(), [gen_statem:reply_action()]}.

add_invokation_listener(From, Ref, State = #state{strict     = Strict,
                                                  strict_log = StrictSucc}) ->
    %% if the invokation does not exist, check the strict_history
    case lists:keyfind(Ref, #expectation.id, Strict) of
        false ->
            case lists:keyfind(Ref, #strict_log.eref, StrictSucc) of
                false ->
                    {State, [{reply, From, {error, invalid_handle}}]};

                #strict_log{ args = Args,
                             ipid = IPid
                           } ->
                    {State, [{reply, From, {success, IPid, Args}}]}
            end;

        E = #expectation{listeners = Ls} ->
            NewE = E#expectation{listeners = [From|Ls]},
            NewStrict = lists:keyreplace(Ref, 2, Strict, NewE),
            {State#state{strict = NewStrict}, []}
    end.
%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
load_original_module(Mod) ->
    really_delete(Mod),
    case code:load_file(Mod) of
        {module, Mod} ->
            existing_module;
        _ ->
            fantasy_module
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
assert_mocked_function_exists(#expectation{m = Mod, f = Fun, a = Args}) ->
            case erlang:function_exported(Mod, Fun, length(Args)) of
                false ->
                    throw({'_______________em_invalid_mock_program_______________',
                           lists:flatten(
                             io_lib:format(
                               "erly_mock: mocked function not exported: ~w:~w/~w",
                               [Mod, Fun, length(Args)])),
                           Args});
                true ->
                    ok
            end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
get_next_expectations(#state{strict = Es, stub = StubEs}) ->
    heads_by_group_tag(Es) ++ StubEs.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
heads_by_group_tag(Es) ->
    lists:foldl(fun
                   (#expectation{g = EG}, Acc = [#expectation{g = AG}|_])
                      when EG =:= AG ->
                        Acc;

                    (E, Acc) ->
                        [E|Acc]
                end,
                [],
                lists:keysort(#expectation.g, Es)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
find_matching_expectation(I, Hints, []) ->
    {error, {unexpected_invokation, I, Hints}};

find_matching_expectation(I = {invokation, Mod, Fun, Args, IPid},
                          Hints,
                          [E|RestEs]) ->
    case E of
        #expectation{m = Mod,
                     f = Fun,
                     a = EArgs}
        when
              length(EArgs) == length(Args) ->
            case check_args(Args, EArgs, IPid) of
                true ->
                    {ok, E};

                {error, Index, Expected, Actual} ->
                    Hint = {parameter_mismatch,
                            {parameter, Index},
                            {expected, Expected},
                            {actual, Actual},
                            E},
                    find_matching_expectation(I, [Hint|Hints], RestEs)
            end;

        _ ->
            Hint = {mfa_mismatch, E},
            find_matching_expectation(I, [Hint|Hints], RestEs)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec answer_invokation(Invokation :: term(),
                        #expectation{},
                        gen_statem:from()) ->
    [gen_statem:reply_action()].

answer_invokation({invokation, _Mod, _Fun, Args, IPid},
                  #expectation{answer    = Answer,
                               listeners = Listeners},
                  From) ->
    [{reply, From, Answer}|
      [{reply, Listener, {success, IPid, Args}} || Listener <- Listeners]].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
remove_expectation(#expectation{id = EId},
                   St = #state{strict = Stricts}) ->

    St#state {
      strict = lists:keydelete(
                 EId,
                 #expectation.id,
                 Stricts)
     }.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
log_invokation({invokation, Mod, Fun, Args, IPid},
               #expectation {
                 id = EId,
                 g = GroupTag,
                 answer = Answer
                },
               St = #state{ strict_log = Log,
                            call_log   = CallLog }) ->
    St#state {
      strict_log = [#strict_log {
                      eref = EId,
                       grpt = GroupTag,
                       ipid = IPid,
                       args = Args
                      } | Log],
      call_log = [{Mod, Fun, Args, Answer} | CallLog]}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec stop_or_continue_replay([gen_statem:reply_action()], statedata()) ->
        gen_statem:event_handler_result(no_expectations).
stop_or_continue_replay(
                    ReplyActions,
                    St = #state{
                                strict      = Expectations,
                                on_finished = OnFinished
                               }) ->
    case {Expectations, OnFinished} of

        {[], undefined} ->
            {next_state, no_expectations, St,
             [reset_invokation_timer()|ReplyActions]};

        {[_|_], _} ->
            {keep_state, St, [start_invokation_timer(St)|ReplyActions]};

        {[], OnFinished} ->
            {stop_and_reply, normal, ReplyActions++[{reply, OnFinished, ok}],
             St}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
set_invokation_timeout(InvTimeout, S = #state{}) ->
    S#state{ inv_to = InvTimeout }.

-define(set_invokation_timeout_action(T),
        {{timeout, invokation_timeout}, T, invokation_timeout}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec start_invokation_timer(statedata()) -> gen_statem:enter_action().
start_invokation_timer(#state{ inv_to = InvTo }) ->
    ?set_invokation_timeout_action(InvTo).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec reset_invokation_timer() -> gen_statem:enter_action().
reset_invokation_timer() -> ?set_invokation_timeout_action(infinity).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec enter_deranged([gen_statem:reply_action()], Error :: term(), #state{}) ->
                       gen_statem:event_handler_result(deranged).

enter_deranged(ReplyActions, What, State = #state{ error  = no_error,
                                                   strict = Strict}) ->
    {next_state,
     deranged,
     State#state{ error = What },
     [reset_invokation_timer()
     |
        [{reply, L, {error, mock_deranged}} ||
                #expectation{ listeners = Ls } <- Strict,
                L <- Ls]
        ++ ReplyActions]}.
