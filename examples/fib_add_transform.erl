-module(fib_add_transform).

-export([parse_transform/2]).

parse_transform(AST, Options) ->
  io:format("AST = ~p~n", [AST]),
  % Parse the incomming AST
  PT_AST = pt_helpers:parse(AST, Options),
  
  % (0) -> 0
  FibClause0 = pt_helpers:build_clause(
        pt_helpers:build_integer(0),
        pt_helpers:build_integer(0)
      ),

  % (1) -> 1
  FibClause1 = pt_helpers:build_clause(
        pt_helpers:build_integer(1),
        pt_helpers:build_integer(1)
      ),

  % N
  FibVar = pt_helpers:build_var('N'),
  % fibonacci(N - 1)
  FibCallN1 = pt_helpers:build_call(fibonacci, pt_helpers:build_op('-', FibVar, pt_helpers:build_integer(1))),
  % fibonacci(N - 2)
  FibCallN2 = pt_helpers:build_call(fibonacci, pt_helpers:build_op('-', FibVar, pt_helpers:build_integer(2))),
  % is_integer(N)
  GuardIsInteger = pt_helpers:build_call(is_integer, [FibVar]),
  % N >= 0
  GuardIsPositive = pt_helpers:build_op('>=', FibVar, pt_helpers:build_integer(0)),
  % (N) when is_integer(N), N >= 0 -> fibonacci(N - 1) + fibonacci(N - 2) 
  FibClauseN =  pt_helpers:build_clause(
                  FibVar, 
                  pt_helpers:build_and_guard(GuardIsInteger, GuardIsPositive),
                  pt_helpers:build_op('+', FibCallN1, FibCallN2)
                ),

  % fibonacci(0) -> 0;
  % fibonacci(1) -> 1;
  % fibonacci(N) when is_integer(N), N >= 0 -> fibonacci(N - 1) + fibonacci(N - 1).
  PT_AST1 = pt_helpers:add_function(PT_AST, export, fibonacci, [FibClause0, FibClause1, FibClauseN]),

  % Generate the output AST
  pt_helpers:generate(PT_AST1).
