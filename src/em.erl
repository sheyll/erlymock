%%%-----------------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2011, 2012, 2013, 2014 Sven Heyll
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
%%% await_groups/1} with a list of groups to block the caller until all groups
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
%%% Copyright (c) 2011,2012,2013 Sven Heyll
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

-behaviour(gen_fsm).

%% public API ---
-export([new/0,
         new_groups/2,
	 nothing/2,
         lock/2,
         strict/4,
         strict/5,
         any/0,
         zelf/0,
         stub/4,
         stub/5,
         replay/1,
         await/2,
   %      await_groups/1,
         await_expectations/1,
         verify/1,
         call_log/1]).

%% gen_fsm callbacks ---
-export([programming/3,
         replaying/2,
         replaying/3,
         no_expectations/3,
         deranged/3,
         terminate/3,
         init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4]).

%% !!!NEVER CALL THIS FUNCTION!!! ---
-export([invoke/4]).

-export_type([group/0, group_tag/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% next invokation must happen in this time, orelse ... BOOM!!!111oneoneeleventy
-define(INVOKATION_TIMEOUT, 4020).

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
%% A group is a pair with a tag for a group and a mock process.
%% ------------------------------------------------------------------------------
-type group_tag() :: {term(), reference()}.
-type group() :: {group, pid(), group_tag()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% API
%%

%%------------------------------------------------------------------------------
%% @doc
%% <p>Spawn a linked mock process and returns it's pid. This is usually the
%% first thing to do in each unit test. The resulting pid is used in the other
%% functions below.</p> <p>NOTE: only a single mock proccess is required for a
%% single unit test case. One mock process can mock an arbitrary number of
%% different modules.</p> <p>When the mock process dies, all uploaded modules
%% are purged from the code server, and all cover compiled modules are
%% restored.</p> <p>When the process that started the mock exits, the mock
%% automatically cleans up and exits.</p> <p>After new() the mock is in
%% 'programming' state.</p>
%% @end
%%------------------------------------------------------------------------------
-spec new() ->
                 group().
new() ->
    {ok, M} = gen_fsm:start_link(?MODULE, [erlang:self()], []),
    RootTag = {root, make_ref()},
    {group, M, RootTag}.

%%------------------------------------------------------------------------------
%% @doc
%% Create a group handle to assign mock expectation to. The result can be passed
%% to {@link strict/4} or {@link strict/5} and {@link await_groups/1}.
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
%% <p>All expectations defined by 'strict' define an order in which the
%% application must call the mocked functions, hence the name 'strict' as oposed
%% to 'stub' (see below).</p>
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
%% <p>This function returns a term that identifies this expectations so that the
%% code under test can call {@link await/2} to block until this expectation is
%% fullfilled.</p>
%% <p>
%% The return value, that the application will get when calling the mocked
%% function in the replay phase is simply the atom <code>ok</code>. This
%% differentiates this function from {@link strict/5}, which allows the
%% definition of a custom response function or a custom return value.  </p>
%% NOTE: This function may only be called between <code>new/0</code> and {@link
%% replay/1} - that is during the programming phase.
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
    gen_fsm:sync_send_event(M, {strict, Group, Mod, Fun, Args, Answer});

strict({group, M, Group}, Mod, Fun, Args, Answer = {function, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    gen_fsm:sync_send_event(M, {strict, Group, Mod, Fun, Args, Answer}).

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
stub({group, M, {root, _}}, Mod, Fun, Args, Answer = {return, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {stub, Mod, Fun, Args, Answer});

stub({group, M, {root, _}}, Mod, Fun, Args, Answer = {function, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {stub, Mod, Fun, Args, Answer}).

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
   ok = gen_fsm:sync_send_event(M, {nothing, Mod}).

%%------------------------------------------------------------------------------
%% @doc
%% The function will block until all modules in the list are not
%% mocked by another erlymock process.
%% @end
%%------------------------------------------------------------------------------
-spec lock(group(), [atom()]) ->
		     ok.
lock({group, M, {root, _}}, Mods) when is_pid(M), is_list(Mods) ->
   ok = em_module_locker:lock(M, Mods).

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
replay({group, M, {root, _}}) ->
    ok = gen_fsm:sync_send_event(M, replay).

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
    gen_fsm:sync_send_event(M, {await, Handle}, 5000).

%%------------------------------------------------------------------------------
%% @doc
%% Retrieve a list of successfully mocked invokations, i.e. alls call that were
%% accepted by the `em' processin in the `replay' phase. Noth strict and stub
%% invokations are recorded.  NOTE: The Answer might as well be a function,
%% depending on the `return' argument passed to `strict' or `stub'.
%% @end
%%------------------------------------------------------------------------------
-spec call_log(group()) ->  [{Mod :: atom(),
                              Func :: atom(),
                              Args :: [term()],
                              Answer :: term()}].
call_log({group, M, {root, _}}) ->
    gen_fsm:sync_send_all_state_event(M, get_call_log).

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
        gen_fsm:sync_send_event(M, await_expectations, 5000)
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
        gen_fsm:sync_send_event(M, verify, infinity)
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
         inv_to_ref       :: reference(),
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
         error = no_error :: no_error | term()
        }).

-type statedata() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% gen_fsm callbacks
%%i

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec init([TestProc :: term()]) ->
                  {ok, atom(), StateData :: statedata()}.
init([TestProc]) ->
    process_flag(sensitive, true),
    erlang:trace(self(), false, [all]),
    {ok,
     programming,
     #state{
        test_proc = TestProc,
        strict = [],
        strict_log = [],
        stub = [],
		call_log =[],
        blacklist = [],
        mocked_modules = []}}.


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec programming(Event :: term(), From :: term(), State :: statedata()) ->
                         {reply, Reply :: term(), NextState :: atom(),
                          NewStateData :: statedata()}.
programming({strict, Group, Mod, Fun, Args, Answer},
            _From,
            State = #state{strict = Strict}) ->
    InvRef = make_ref(),
    {reply,
     InvRef,
     programming,
     State#state{
       strict = [#expectation{id = InvRef,
                              g = Group,
                              m = Mod,
                              f = Fun,
                              a = Args,
                              answer = Answer,
                              listeners = []}
                 |Strict]}};

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
    InvTORef = gen_fsm:start_timer(?INVOKATION_TIMEOUT, invokation_timeout),
    {reply,
     ok,
     case Strict of
         [] -> no_expectations;
         _ -> replaying
     end,
     State#state{
       inv_to_ref = InvTORef,
       strict = lists:reverse(Strict),
       mocked_modules = MMs}};

programming(Event, _From, State) ->
    {reply, {error, {bad_request, programming, Event}}, programming, State}.



%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec replaying(Event :: term(), StateData :: statedata()) ->
                       {stop, Reason :: term(), NewStateData :: statedata()}.
replaying({timeout, Ref, invokation_timeout},
          State = #state{inv_to_ref = Ref,
                         strict = Expectations}) ->
    {stop, {invokation_timeout, {missing_invokations, Expectations}}, State}.

-spec replaying(Event :: term(), From :: term(), StateData :: statedata()) ->
                       {next_state, NextState :: atom(),
                        NewStateData :: statedata()} |
                       {reply, Reply :: term(), NextState :: atom(),
                        NewStateData :: statedata()} |
                       {stop, Reason :: term(), Reply :: term(),
                        NewStateData :: statedata()}.

replaying(Inv = {invokation, _M, _F, _A, _IPid}, From, State) ->
    handle_invokation(Inv, From, State);

replaying(verify, _From, State = #state{inv_to_ref = InvTORef}) ->
    gen_fsm:cancel_timer(InvTORef),
    Reason = {invokations_missing, State#state.strict},
    {stop, normal, Reason, State};

replaying({await, H}, From, State) ->
    {next_state, replaying, add_invokation_listener(From, H, State)};

replaying(await_expectations, From, State = #state{on_finished = undefined}) ->
    {next_state, replaying, State#state{ on_finished = From }};

replaying(Event, _From, State) ->
    {reply, {error, {bad_request, replaying, Event}}, replaying, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec no_expectations(Event     :: term(),
                      From      :: term(),
                      StateData :: statedata()) ->
                             {reply,
                              Reply        :: term(),
                              NextState    :: atom(),
                              NewStateData :: statedata()}.
no_expectations(I = {invokation, M, F, A, _IPid},
                From,
                State = #state{call_log = CallLog}) ->
    case handle_stub_invokation(I, State#state.stub) of
        {ok, Answer} ->
            {reply, Answer, no_expectations,
             State#state{call_log = [{M, F, A, Answer}|CallLog]}};

        error ->
            Error = {unexpected_invokation, I},
            gen_fsm:reply(From, {'$em_error', Error}),
            set_deranged(Error, State)
    end;

no_expectations(verify, _From, State) ->
    {stop, normal, ok, State};

no_expectations(await_expectations, _From, State) ->
    {stop, normal, ok, State};

no_expectations({await, H}, From, State) ->
    {next_state, no_expectations, add_invokation_listener(From, H, State)};

no_expectations(Event, _From, State) ->
    {reply,
     {error, {bad_request, no_expectations, Event}},
     no_expectations,
     State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec deranged(Event     :: term(),
               From      :: term(),
               StateData :: statedata()) ->
                      {reply,
                       Reply        :: term(),
                       NextState    :: atom(),
                       NewStateData :: statedata()} |
                      {stop,
                       Reason       :: term(),
                       Reply        :: term(),
                       NewStateData :: statedata()}.

deranged(verify, _From, State = #state{ error = Error }) ->
    {stop, normal, Error, State};

deranged(await_expectations, _From, State = #state{ error = Error }) ->
    {stop, normal, Error, State};

deranged({invokation, _M, _F, _A, _IPid}, _From, State) ->
    {reply, {'$em_error', mock_deranged}, deranged, State};

deranged({await, _}, _From, State) ->
    {reply, {error, mock_deranged}, deranged, State};

deranged(Event, _From, State) ->
    {reply, {error, {bad_request, deranged, Event}}, deranged, State}.


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec terminate(Reason :: term(), StateName :: atom(),
                StateData :: statedata()) -> no_return().
terminate(_Reason, _StateName, State) ->
    (catch unload_mock_modules(State)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec code_change(OldVsn :: term(), StateName :: atom(), State :: statedata(),
                  Extra :: term()) ->
                         {ok, NextState :: atom(), NewStateData :: statedata()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_sync_event(Event :: term(), From :: term(), StateName :: atom(),
                        StateData :: statedata()) ->
                               {reply,
                                Reply     :: term(),
                                StateName :: atom(),
                                StateData :: statedata()}.
handle_sync_event(get_call_log, _From, StateName, State) ->
    {reply, lists:reverse(State#state.call_log), StateName, State};
handle_sync_event(_Evt, _From, StateName, State) ->
    {reply, {error, bad_request}, StateName, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_info(Info :: term(), StateName :: atom(),
                  StateData :: statedata()) ->
                         {stop, normal, NewStateData :: statedata()}.
handle_info(_Info, _StateName, State) ->
    {stop, normal, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_event(Msg :: term(), StateName :: atom(),
                   StateData :: statedata()) ->
                          {stop, normal, NewStateData :: statedata()}.
handle_event(_Msg, _StateName, State) ->
    {stop, normal, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% api for generated mock code
%%

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec invoke(M :: term(), Mod :: term(), Fun :: fun(), Args :: list()) ->
                    {Value :: term()}.
invoke(M, Mod, Fun, Args) ->
    (catch io:format("~nEM: ~w:~w ~p",[Mod,Fun, Args])),
    Trace = erlang:get_stacktrace(),
    Rv = case gen_fsm:sync_send_event(M, {invokation, Mod, Fun, Args, self()}) of
             {return, Value} ->
                 Value;
             {'$em_error' , WTF} ->
                 (catch io:format(" *ERROR* ->  ~p~nAT: ~p~n~n",[WTF, Trace])),
                 exit({mock_error, WTF});
             {function, F} ->
                 F(Args)
         end,
    (catch io:format(" ->  ~p~n",[Rv])),
    Rv.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% internal functions
%%
%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
unload_mock_modules(#state{mocked_modules = MMs}) ->
    [begin
         code:purge(Mod),
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
    em_module_locker:lock(erlang:self(), ModulesToMock),
    [check_func(Ex) || Ex <- Expectations],
    [install_mock_module(M, Expectations) || M <- ModulesToMock].

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
install_mock_module(Mod, Expectations) ->
    MaybeBin = get_cover_compiled_binary(Mod),
    ModHeaderSyn = [erl_syntax:attribute(erl_syntax:atom(module),
					 [erl_syntax:atom(Mod)]),
                    erl_syntax:attribute(erl_syntax:atom(erlymock_generated),
					 [erl_syntax:atom(true)]),
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

    code:purge(Mod),
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
handle_stub_invokation({invokation, Mod, Fun, Args, IPid}, Stubs) ->
    case [MatchingStub
          || MatchingStub = #expectation {m = M, f = F, a = A} <- Stubs,
             M == Mod, F == Fun, length(Args) == length(A),
             check_args(Args, A, IPid) == true] of

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

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
add_invokation_listener(From, Ref, State = #state{strict     = Strict,
                                                  strict_log = StrictSucc}) ->
    %% if the invokation does not exist, check the strict_history
    case lists:keyfind(Ref, #expectation.id, Strict) of
        false ->
            case lists:keyfind(Ref, #strict_log.eref, StrictSucc) of
                false ->
                    gen_fsm:reply(From, {error, invalid_handle});

                #strict_log{ args = Args,
                             ipid = IPid
                           } ->
                    gen_fsm:reply(From, {success, IPid, Args})
            end,
            State;

        E = #expectation{listeners = Ls} ->
            NewE = E#expectation{listeners = [From|Ls]},
            NewStrict = lists:keyreplace(Ref, 2, Strict, NewE),
            State#state{strict = NewStrict}
    end.


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_func(#expectation{m = Mod, f = Fun, a = Args}) ->
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    case code:load_file(Mod) of
        {module, Mod} ->
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
            end;

        _ ->
            ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
handle_invokation(Inv, From, St0) ->
    St1 = cancel_invokation_timer(St0),
    case
        find_matching_expectation(
          Inv,
          [],
          get_next_expectations(St1))
    of
        {ok, E = #expectation{}} ->
            answer_invokation(Inv, E, From),
            stop_or_continue_replay(
              remove_expectation(
                E,
                log_invokation(Inv, E, St1)));

        {error, Error} ->
            gen_fsm:reply(From, {'$em_error', Error}),
            set_deranged(Error, St1)
    end.

%% ----------------------------------------------------------------------

cancel_invokation_timer(St = #state{inv_to_ref = undefined}) ->
    St;

cancel_invokation_timer(St = #state{inv_to_ref = TimerRef}) ->
    gen_fsm:cancel_timer(TimerRef),
    St#state{inv_to_ref = undefined}.

%% ----------------------------------------------------------------------

get_next_expectations(#state{strict = Es, stub = StubEs}) ->
    heads_by_group_tag(Es) ++ StubEs.

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

%% ----------------------------------------------------------------------

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

%% ----------------------------------------------------------------------

answer_invokation({invokation, _Mod, _Fun, Args, IPid},
                  #expectation{answer    = Answer,
                               listeners = Listeners},
                  From) ->
    gen_fsm:reply(From, Answer),
    [gen_fsm:reply(Listener, {success, IPid, Args}) || Listener <- Listeners].

%% ----------------------------------------------------------------------

remove_expectation(#expectation{id = EId},
                   St = #state{strict = Stricts}) ->

    St#state {
      strict = lists:keydelete(
                 EId,
                 #expectation.id,
                 Stricts)
     }.

%% ----------------------------------------------------------------------

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

%% ----------------------------------------------------------------------

stop_or_continue_replay(St = #state{
                          strict = Expectations,
                          on_finished = OnFinished
                         }) ->
    case {Expectations, OnFinished} of

        {[], undefined} ->
            {next_state, no_expectations, St};

        {[_|_], _} ->
            St1 = start_invokation_timer(St),
            {next_state, replaying, St1};

        {[], OnFinished} ->
            gen_fsm:reply(OnFinished, ok),
            {stop, normal, St}
    end.

%% ----------------------------------------------------------------------
%% TODO use this function more often
start_invokation_timer(St) ->
    NextTimer = gen_fsm:start_timer(?INVOKATION_TIMEOUT,
                                    invokation_timeout),
    St#state{inv_to_ref = NextTimer}.


%% ----------------------------------------------------------------------

-spec set_deranged(term(), #state{}) ->
                       {next_state, deranged, #state{}}.

set_deranged(What, State = #state{ error  = no_error,
                                   strict = Strict}) ->
    [gen_fsm:reply(L, {error, mock_deranged}) ||
        #expectation{ listeners = Ls } <- Strict,
        L <- Ls],

    {next_state,
     deranged,
     cancel_invokation_timer(
       State#state{ error = What })}.
