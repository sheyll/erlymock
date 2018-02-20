-ifndef(em_hrl).
-define(em_hrl, true).

-define(MATCH(XXX), (fun(XXX) -> true; (_) -> false end)).
-define(ERLYMOCK_COMPILED, erlymock_compiled).
-define(em_mocking_in_progress, em_mocking_in_progress).
-define(em_internal_module_loading_section, em_internal_module_loading_section).

-endif.
