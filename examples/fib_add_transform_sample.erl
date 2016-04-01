-module(fib_add_transform_sample).
-compile([{parse_transform, fib_add_transform2}]).

-export([fib/1]).
-include("../include/pt_helpers.hrl").
-define(TOTO, 1).
-facts([{a, b}]).

% @doc
% fibo
% @end
-spec fib(N :: integer()) -> integer().
fib(0) -> 0;
fib(?TOTO) -> ?TOTO;
fib(N) when is_integer(N), N >= 0 -> 
  fib(N - 1) + fib(N - 2).
