-module(fib_add_transform_sample).
-compile([{parse_transform, fib_add_transform}]).

-export([fib/1]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) when is_integer(N), N >= 0 -> 
  fib(N - 1) + fib(N - 2).
