%%%=============================================================================
%%%
%%% @author Sven Heyll <sven.heyll@lindenbaum.eu>
%%% @copyright (C) 2011, Sven Heyll
%%%
%%% @doc
%%% Internal server that provides a locking mechanism between em processes so that
%%% no two mock processes mock the same modules.
%%% A mock process locks some modules and as soon as the mock process dies, its
%%% modules will automatically be unlocked.
%%% @end
%%%=============================================================================

-module(em_module_locker).

-behaviour(gen_server).

%% API
-export([lock/2]).

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
         timers         = dict:new(),
         locking_mocks  = [] :: [{pid(), [module()], term()}],
         waiting_mocks  = [] :: [{pid(), [module()], term()}]}).

-type state() :: #state{}.

%%------------------------------------------------------------------------------
%% @doc
%% The maximum amount of time, that a process may lock modules before being
%% brutally killed.
%% @end
%%------------------------------------------------------------------------------
-define(MAX_LOCK_TIME, 4400).

%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc
%% Blocks until the modules in the list can be mocked.
%% @end
%%------------------------------------------------------------------------------
-spec lock(pid(), [module()]) ->
                  ok.
lock(MockPid, Mods) ->
    case whereis(?MODULE) of
        undefined -> gen_server:start({local, ?SERVER}, ?MODULE, [], []);
        _ ->
            ok
    end,
    gen_server:call(?MODULE, {lock, MockPid, Mods}, infinity).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec init(Args :: []) -> {ok, state()}.
init([]) ->
    {ok, #state{}}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: term(), State :: state()) ->
                         {reply, Reply :: term(), NewState :: state()} |
                         {stop, term(), term(), term()}.
handle_call({lock, MockPid, Mods}, From,
            State = #state{}) ->
    monitor(process, MockPid),
    NewState = State#state{waiting_mocks = [{MockPid, Mods, From} | State#state.waiting_mocks]},
    em_module_locker:handle_cast(perform_locking, NewState);

handle_call(Request, _From, State) ->
    {stop, {undefined, Request}, {unexpected_call, Request}, State}.

-spec handle_cast(Msg :: term(), State :: state()) ->
                         {noreply, NewState :: state()} |
                         {stop, term(), term()}.
handle_cast(perform_locking,
            State) ->
    Mocks_To_Reply = [M || M = {_, Mods, _} <- State#state.waiting_mocks,
                           (Mods -- (State#state.locked_modules)) == Mods],
    NewState = lists:foldr(fun(M = {MockPid, Mods, From}, StateAcc) ->
                                   {ok, TRef} = timer:kill_after(?MAX_LOCK_TIME, MockPid),
                                   gen_server:reply(From, ok),
                                   StateAcc#state{
                                     timers =
                                         dict:store(MockPid, TRef, StateAcc#state.timers),
                                     locked_modules =
                                         StateAcc#state.locked_modules ++ Mods,
                                     waiting_mocks =
                                         StateAcc#state.waiting_mocks -- [M],
                                     locking_mocks =
                                         [M | StateAcc#state.locking_mocks]}
                           end,
                           State,
                           Mocks_To_Reply),
    {noreply, NewState};

handle_cast(Request, State) ->
    {stop, {unexpected_cast, Request}, State}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: state()) ->
                         {noreply, State :: state()} | {stop, term(), term()}.
handle_info({'DOWN', _Ref, process, Pid, _Reason},
            StateWithTimers = #state{timers = Timers}) ->
    State = case dict:find(Pid, Timers) of
                {ok, TRef} ->
                    timer:cancel(TRef),
                    StateWithTimers#state{timers = dict:erase(Pid, Timers)};
                _ ->
                    StateWithTimers
    end,
    Mocks_To_Unlock = [M || M = {P, _, _} <- State#state.locking_mocks,
                            P == Pid],
    Mods_To_Unlock = [Mod || {_, Mods, _} <- Mocks_To_Unlock,
                            Mod <- Mods],

    Mocks_Not_Waiting_Anymore = [M || M = {P, _, _} <- State#state.waiting_mocks,
                                     P == Pid],
    gen_server:cast(?MODULE, perform_locking),
    {noreply,
     State#state{locked_modules = State#state.locked_modules -- Mods_To_Unlock,
                 locking_mocks = State#state.locking_mocks -- Mocks_To_Unlock,
                 waiting_mocks = State#state.waiting_mocks -- Mocks_Not_Waiting_Anymore}};

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
