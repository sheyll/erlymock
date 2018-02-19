%%%=============================================================================
%%%
%%% @author Sven Heyll <sven.heyll@gmail.com>
%%% @copyright (C) 2011, 2014 Sven Heyll
%%%
%%% @doc
%%% Internal mutex process to serialize the access to modules being loaded by
%%% concurrent tests.
%%% There is an inherent race condition between checking if the module attribute
%%% ERLYMOCK_COMPILED is already set on a module and then loading that module,
%%% which can only be resolved by atomically checking and locking the module(s)
%%% being overwritten.
%%% @end
%%%=============================================================================

-module(em_mocking_queue).

-behaviour(gen_server).
-include_lib("em/include/em.hrl").

%% API
-export([enqueue_and_wait/2,
         lock_immediately/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {locked_modules = [] :: [module()],
         locking_mocks  = [] :: [{pid(), [module()], term()}],
         waiting_mocks  = [] :: [{pid(), [module()], term()}]}).

-type state() :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc
%% Checks for the ERLYMOCK_COMPILED module attribute on the given modules and
%% marks the modules as "locked".
%% @end
%%------------------------------------------------------------------------------
-spec enqueue_and_wait(pid(), [module()]) ->
                  ok.
enqueue_and_wait(MockPid, Mods) ->
    case whereis(?MODULE) of
        undefined -> gen_server:start({local, ?SERVER}, ?MODULE, [], []);
        _ ->
            ok
    end,
    gen_server:call(?MODULE, {enqueue_and_wait, MockPid, Mods}, infinity).


%%------------------------------------------------------------------------------
%% @doc
%% Atomically lock a global record on any module
%% @end
%%------------------------------------------------------------------------------
-spec lock_immediately(pid(), [module()]) ->
                  ok.
lock_immediately(MockPid, Mods) ->
    case whereis(?MODULE) of
        undefined -> gen_server:start({local, ?SERVER}, ?MODULE, [], []);
        _ ->
            ok
    end,
    gen_server:call(?MODULE, {lock_immediately, MockPid, Mods}, infinity).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec init(Args :: []) -> {ok, state()}.
init([]) ->
    process_flag(sensitive, true),
    erlang:trace(self(), false, [all]),
    {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: term(), State :: state()) ->
                         {reply, Reply :: term(), NewState :: state()} |
                         {stop, term(), term(), term()}.
handle_call({enqueue_and_wait, MockPid, Mods}, From,
            State = #state{}) ->
    monitor(process, MockPid),
    NewWaiting = [{MockPid, Mods, From} | State#state.waiting_mocks],
    NewState = State#state{ waiting_mocks = NewWaiting},
    {noreply, perform_locking(NewState)};

handle_call({lock_immediately, MockPid, Mods}, From, State) ->
    case lock_that({MockPid, Mods, From}, State) of
        {reply, From, StateOut} ->
            monitor(process, MockPid),
            {reply, ok, StateOut};
        {noreply, _StateOut} ->
            {reply, {error,
                     {modules_already_mocked,
                      Mods -- State#state.locked_modules}},
             State}
    end;

handle_call(Request, _From, State) ->
    {stop, {undefined, Request}, {unexpected_call, Request}, State}.

handle_cast(Request, State) ->
    {stop, {unexpected_cast, Request}, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: state()) ->
                         {noreply, State :: state()} | {stop, term(), term()}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    Mocks_To_Unlock = [M || M = {P, _, _} <- State#state.locking_mocks,
                            P == Pid],
    Mods_To_Unlock = [Mod || {_, Mods, _} <- Mocks_To_Unlock,
                            Mod <- Mods],
    Mocks_Not_Waiting_Anymore = [M || M = {P, _, _} <- State#state.waiting_mocks,
                                     P == Pid],
    {noreply,
     perform_locking(State#state{locked_modules = State#state.locked_modules -- Mods_To_Unlock,
                                 locking_mocks  = State#state.locking_mocks -- Mocks_To_Unlock,
                                 waiting_mocks  = State#state.waiting_mocks -- Mocks_Not_Waiting_Anymore})};

handle_info(Info, State) ->
    {stop, {unexpected_info, Info}, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: state(), Extra :: term()) ->
                         {ok, NewState :: state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

perform_locking(State) ->
    lists:foldr(fun(M, StateAcc) ->
                        case lock_that(M, StateAcc) of
                            {reply, From, StateAccOut} ->
                                gen_server:reply(From, ok),
                                StateAccOut;
                            {noreply, StateAccOut} ->
                                StateAccOut
                        end
                end,
                State,
                State#state.waiting_mocks).

lock_that(M = {_, Mods, From}, StateAcc) ->
    AllModulesUnlocked =
        (Mods -- (StateAcc#state.locked_modules)) == Mods,
    if AllModulesUnlocked ->
            {reply, From, StateAcc#state{
              locked_modules =
                  StateAcc#state.locked_modules ++ Mods,
              waiting_mocks  =
                  StateAcc#state.waiting_mocks -- [M],
              locking_mocks  =
                  [M | StateAcc#state.locking_mocks]}};
       true ->
            {noreply, StateAcc}
    end.
