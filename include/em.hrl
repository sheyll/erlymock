-ifndef(em_hrl).
-define(em_hrl, true).

-define(MATCH(XXX), (fun(XXX) -> true; (_) -> false end)).

-endif.
