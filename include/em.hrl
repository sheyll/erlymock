-ifndef(em_hrl).
-define(em_hrl, true).

-define(MATCH(XXX), (fun(XXX) -> true; (_) -> false end)).
-define(ERLYMOCK_COMPILED, erlymock_compiled).
-define(EM_GLOBAL_MODULE_LOCK_ID(M, Self),
        {{em_mocked_module, M}, {em_mock_process, Self}}).
-define(EM_GLOBAL_MODULE_LOCK_RETRIES, 3).
-endif.
