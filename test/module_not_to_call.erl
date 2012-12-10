-module(module_not_to_call).

-export([some_fun/1]).

some_fun(some_arg) ->
    ok.
