%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%% @doc
%%% 'em' stands for 'Early Mock'.
%%% A mocking library that works similar to easymock. Code for modules
%%% that should be mocked is created and loaded on the fly.
%%% @end
%%% @copyright (C) 2011, Sven Heyll
%%%-------------------------------------------------------------------
-module(em).

-behaviour(gen_fsm).

-export([new/0,
         strict/4,
         strict/5,
         stub/4,
         stub/5,
         replay/1,
         verify/1,
         any/0]).

-export([programming/3,
         replaying/3,
         no_expectations/3,
         terminate/3,
         init/1]).

%% !!!NEVER CALL THIS FUNCTION!!! ---
-export([invoke/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% important types
%%

-type args() :: [ fun((any()) ->
                             true | false)
                     | term()].

-type answer() :: {function, fun(([any()]) -> any())}
                  | {return, any()} .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% API
%%

-spec any() ->
                 fun((any()) ->
                    true).
any() ->
    fun(_) ->
            true
    end.

-spec new() ->
                 pid().
new() ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [], []),
    Pid.

-spec strict(pid(), atom(), atom(), args()) ->
                    ok.
strict(M, Mod, Fun, Args)
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    strict(M, Mod, Fun, Args, {return, ok}).

-spec strict(pid(), atom(), atom(), args(), answer()) ->
                    ok.
strict(M, Mod, Fun, Args, Answer = {return, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {strict, Mod, Fun, Args, Answer});
strict(M, Mod, Fun, Args, Answer = {function, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {strict, Mod, Fun, Args, Answer}).

-spec stub(pid(), atom(), atom(), args()) ->
                  ok.
stub(M, Mod, Fun, Args)
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    stub(M, Mod, Fun, Args, {return, ok}).

-spec stub(pid(), atom(), atom(), args(), answer()) ->
                  ok.
stub(M, Mod, Fun, Args, Answer = {return, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {stub, Mod, Fun, Args, Answer});
stub(M, Mod, Fun, Args, Answer = {function, _})
  when is_pid(M), is_atom(Mod), is_atom(Fun), is_list(Args) ->
    ok = gen_fsm:sync_send_event(M, {stub, Mod, Fun, Args, Answer}).

replay(M) ->
    ok = gen_fsm:sync_send_event(M, replay).

verify(M) ->
    ok = gen_fsm:sync_send_event(M, verify).

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
          mocked_modules :: [{atom(), {just, term()}|nothing}]
         }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% gen_fsm callbacks
%%
init([]) ->
    {ok,
     programming,
     #state{
       strict = [],
       stub = [],
       mocked_modules = []}}.


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
          State = #state{strict = [E|_]}) ->
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

terminate(_Reason, _StateName, State) ->
    unload_mock_modules(State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% api for generated mock code
%%

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

install_mock_modules(#state{strict = ExpectationsStrict,
                            stub = ExpectationsStub}) ->
    Expectations = ExpectationsStub ++ ExpectationsStrict,
    ModulesToMock = lists:usort([M || #expectation{m = M} <- Expectations]),
    [install_mock_module(M, Expectations) || M <- ModulesToMock].

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


mock_fun_syn(Mod, F, Args) ->
    ArgsSyn = var_list_syn(Args),
    FunSyn = erl_syntax:atom(F),
    erl_syntax:function(
       FunSyn,
       [erl_syntax:clause(ArgsSyn,
                          none,
                          body_syn(Mod, FunSyn, ArgsSyn))]).

var_list_syn(Args) ->
    [erl_syntax:variable(list_to_atom("Arg_" ++ integer_to_list(I)))
     || I <- lists:seq(0, Args - 1)].

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
