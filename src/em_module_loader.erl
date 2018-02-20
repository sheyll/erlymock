%%%-------------------------------------------------------------------
%%% @author Sven Heyll <sven@wega.lbaum.eu>
%%% @copyright (C) 2018, Sven Heyll
%%% @doc
%%% <b>INTERNAL MODULE</b>
%%% Load modules on behalf of the {@link em} module.
%%%
%%% A story why we need this:
%%%
%%% Stardate "Feburary 2018". We have a Heisenbug in the unit test
%%% execution introduced in version 7.0.0.
%%%
%%% After narrowing it down to code loading race conditions, We failed
%%% to fix the problem by using `global:set_lock' around critical sections.
%%% Also, we don't know why.
%%%
%%% This module is an attempt to answer to the situation.
%%%
%%% Q: When the `em' process manages loading and restoring the
%%% mocking and original code, what happens if the `em' process
%%% gets killed after loading a module, and a process tries to execute
%%% functions from that module?
%%%
%%% See this code:
%%% <pre>
%%% -module(yolo_tests).
%%% ...
%%% killing_the_mock_test() ->
%%%    M = em:new(),
%%%    em:strict(M, foo, aaaa, [],
%%%              {function, fun(_) -> exit(kill) end}),
%%%    em:replay(M),
%%%    ?assertEqual(test_result,
%%%                 yolo:do_aaaa_with_foo()),
%%%
%%%    em:verify(M).
%%%
%%% simple_test() ->
%%%    ?assertEqual(ok, yolo:just_do_it()).
%%% </pre>
%%%
%%% The implementation:
%%% <pre>
%%% -module(yolo).
%%% -behaviour(gen_server).
%%% ...
%%% just_do_it() ->
%%%     foo:do_just_it(),
%%%     ok.
%%%
%%% do_aaaa_with_foo() ->
%%%     foo:aaaa().
%%% </pre>
%%%
%%% Without another server that cares about loading and unloading the
%%% modules, there is no way the test above can be fixed without removing
%%% the brutal `kill' in the strict callback.
%%%
%%% @end
%%% Created : 19 Feb 2018 by Sven Heyll <sven@wega.lbaum.eu>
%%%-------------------------------------------------------------------
-module(em_module_loader).

-behaviour(gen_statem).

%% API
-export([start/0,
         load_modules/2,
         restore_modules/0,
         disable_module_loading/0,
         enable_module_loading/0
        ]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([no_modules_loaded/3, modules_loaded/3,
         module_loading_disabled/3, deranged/3]).

-include_lib("em/include/em.hrl").

-define(SERVER, ?MODULE).

-record(no_modules_loaded,
        {}).

-record(modules_loaded,
        {owner :: reference(),
         owner_pid :: pid(),
         mocks :: [module()],
         backup :: [{module(), binary(), file:filename()}]}).


-record(loading_disabled,
        {by :: reference(),
         by_pid :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.
start() ->
    case gen_statem:start({local, ?SERVER}, ?MODULE, [], []) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok;
        Other ->
            throw({'unexpected error starting em_module_loader', Other})
    end.


%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_modules([{module(), file:filename(), binary()}],
                   gen_statem:timeout()) ->
                          ok | error.
load_modules(Modules, Timeout) ->
    gen_statem:call(?SERVER, {load_modules, Modules}, Timeout).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec restore_modules() ->
                          ok | error.
restore_modules() ->
    gen_statem:call(?SERVER, restore_modules, infinity).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec disable_module_loading() ->
                          ok | error.
disable_module_loading() ->
    gen_statem:call(?SERVER, disable_module_loading, infinity).

%%--------------------------------------------------------------------
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec enable_module_loading() ->
                          ok | error.
enable_module_loading() ->
    gen_statem:call(?SERVER, enable_module_loading, infinity).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
                  gen_statem:init_result(atom()).
init([]) ->
    process_flag(trap_exit, true),
    {ok, no_modules_loaded, #no_modules_loaded{}}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec no_modules_loaded(gen_statem:event_type(),
                        Msg :: term(),
                        Data :: #no_modules_loaded{}) ->
                               gen_statem:event_handler_result(atom()).

no_modules_loaded({call, Caller = {CallerPid, _}},
                  {load_modules, Modules},
                  #no_modules_loaded{}) ->
    try do_load(Modules) of
        {Mocks, Backup} ->
            dbgLog("Modules Loaded: ~p~n", [Mocks]),
            MonitorRef = erlang:monitor(process, CallerPid),
            {next_state, modules_loaded,
             #modules_loaded{owner=MonitorRef,
                             owner_pid=CallerPid,
                             backup=Backup,
                             mocks=Mocks},
             [{reply, Caller, ok}]}
    catch
        C:E ->
            dbgLog("Load Error: ~p ~p~n", [C,E]),
            {next_state, deranged,
             #no_modules_loaded{},
             [{reply, Caller, error}]}
    end;

no_modules_loaded({call, Caller},
                  restore_modules,
                  #no_modules_loaded{}) ->
    {keep_state_and_data, [{reply, Caller, ok}]};

no_modules_loaded({call, Caller = {CallerPid, _}},
                  disable_module_loading,
                  #no_modules_loaded{}) ->
    MonitorRef = erlang:monitor(process, CallerPid),
    {next_state,
     module_loading_disabled,
     #loading_disabled{by = MonitorRef, by_pid = CallerPid},
     [{reply, Caller, ok}]};

no_modules_loaded(_, _, #no_modules_loaded{}) ->
    {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
module_loading_disabled({call, Caller = {CallerPid,_}},
                        enable_module_loading,
                        #loading_disabled{by = MonitorRef,
                                          by_pid = CallerPid}) ->
    erlang:demonitor(MonitorRef),
    {next_state, no_modules_loaded, #no_modules_loaded{},
     [{reply, Caller,ok}]};
module_loading_disabled(info, {'DOWN', MonitorRef, process, _Pid, _Reason},
                        #loading_disabled{by = MonitorRef}) ->
    {next_state, no_modules_loaded, #no_modules_loaded{},  []};
module_loading_disabled(_,_,_) ->
    {keep_state_and_data, [postpone]}.


%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
deranged({call, Caller},_,_) ->
    {keep_state_and_data,
     [{reply, Caller,
       {error, 'erly-mock moduler loading server internal error, please report this error'}}]};
deranged(_,_,_) ->
    {keep_state_and_data, [postpone]}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------

modules_loaded({call, Caller = {CallerPid, _}},
               restore_modules,
               #modules_loaded{owner = MonitorRef,
                               owner_pid = CallerPid,
                               mocks = Mocks,
                               backup = Backup}) ->
    erlang:demonitor(MonitorRef),
    try do_restore(Mocks, Backup) of
        _ ->
            {next_state, no_modules_loaded, #no_modules_loaded{},
             [{reply, Caller, ok}]}
    catch
        C:E ->
            dbgLog("Restore Error: ~p ~p~n", [C,E]),
            {next_state, deranged,
             #no_modules_loaded{},
             [{reply, Caller, error}]}
    end;
modules_loaded(info,
               {'DOWN', MonitorRef, process, _Pid, _Reason},
               #modules_loaded{owner = MonitorRef,
                               mocks = Mocks,
                               backup = Backup}) ->
    try do_restore(Mocks, Backup) of
        _ ->
            {next_state,
             no_modules_loaded,
             #no_modules_loaded{},
             []}
    catch
        C:E ->
            dbgLog("Restore Error: ~p ~p~n", [C,E]),
            {next_state, deranged, #no_modules_loaded{}, []}
    end;
modules_loaded(_, _, _) ->
    {keep_state_and_data, postpone}.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
                       any().
terminate(_Reason, _State, _Data) ->
    void.

%%--------------------------------------------------------------------
%% @private
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
                         {ok, NewState :: term(), NewData :: term()} |
                         (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_load(Mods) ->
    Backups = [B || {M,_,_} <- Mods,
                    {just, B} <- [backup_and_delete_module(M)]],
    dbgLog("Modules Backed-Up: ~p~n", [[{B,F} || {B,F,_} <- Backups]]),
    do_load_atomically(Mods, 'failed to load mock modules'),
    {[M|| {M,_,_} <- Mods], Backups}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
do_restore(MockMods, BackupMods) ->
    [really_delete(M) || M <- MockMods],
    do_load_atomically(BackupMods, 'failed to restore modules'),
    dbgLog("Modules Deleted: ~p~n", [MockMods]),
    dbgLog("Modules Restored: ~p~n", [[{B,F} || {B,F,_} <- BackupMods]]).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec backup_and_delete_module(module()) ->
                           {just, term()} | nothing.
backup_and_delete_module(Mod) ->
    MaybeBackup = backup_module(Mod),
    really_delete(Mod),
    MaybeBackup.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
really_delete(Mod) ->
    code:purge(Mod),
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod),
    code:delete(Mod),
    code:purge(Mod).


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec backup_module(module()) ->
                           {just, term()} | nothing.
backup_module(Mod) ->
    case code:which(Mod) of
        cover_compiled ->
            case ets:info(cover_binary_code_table) of
                undefined ->
                    nothing;
                _ ->
                    case ets:lookup(cover_binary_code_table, Mod) of
                        [ObjCode] ->
                            dbgLog("Saving: <cover> ~w~n", [Mod]),
                            {just, {Mod, cover_compiled, ObjCode}};
                        _ ->
                            nothing
                    end
            end;
        preloaded ->
            throw({'cannot mock preloaded module', Mod});
        non_existing ->
            nothing;
        [$e,$m,$_,$m,$a,$g,$i,$c,$_|_Rest] ->
            nothing;
        _FName ->
            assert_not_mocked(Mod),
            case code:get_object_code(Mod) of
                {Mod, Binary, FName} ->
                    dbgLog("Saving: ~s ~w~n", [FName, Mod]),
                    {just, {Mod, FName, Binary}};
                error ->
                    throw({'cannot get_object_code for module', Mod})
            end
    end.

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
do_load_atomically(Mods, ErrMsg) ->
    StickyDirs =
        lists:usort([filename:dirname(F) || {M,F,_} <- Mods,
                                            code:is_sticky(M)]),
    [code:unstick_dir(D) || D <- StickyDirs],
    Res = code:atomic_load(Mods),
    [code:stick_dir(D) || D <- StickyDirs],
    case Res of
        ok ->
            ok;
        Errors ->
            throw({ErrMsg, Errors})
    end.


%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-ifdef(EM_DEBUG).
dbgLog(Fmt,Args) ->
    io:format(standard_error,
              "+++++++++++++EM_MODULE_LOADER+++++++++++++ " ++ Fmt,
              Args).
-else.
dbgLog(_Fmt, _Args) -> ok.
-endif.
