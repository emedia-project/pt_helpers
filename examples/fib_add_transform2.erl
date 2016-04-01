-module(fib_add_transform2).

-export([parse_transform/2]).

parse_transform(AST, Options) ->
  io:format("AST = ~p~n", [AST]),
  % Parse the incomming AST
  PT_AST = pt_ast:parse(AST, Options),
  
  % (0) -> 0
  FibClause0 = pt_builder:build_clause(
        pt_builder:build_integer(0),
        pt_builder:build_integer(0)
      ),

  % (1) -> 1
  FibClause1 = pt_builder:build_clause(
        pt_builder:build_integer(1),
        pt_builder:build_integer(1)
      ),

  % N
  FibVar = pt_builder:build_var('N'),
  % fibonacci(N - 1)
  FibCallN1 = pt_builder:build_call(fibonacci, pt_builder:build_op('-', FibVar, pt_builder:build_integer(1))),
  % fibonacci(N - 2)
  FibCallN2 = pt_builder:build_call(fibonacci, pt_builder:build_op('-', FibVar, pt_builder:build_integer(2))),
  % is_integer(N)
  GuardIsInteger = pt_builder:build_call(is_integer, [FibVar]),
  % N >= 0
  GuardIsPositive = pt_builder:build_op('>=', FibVar, pt_builder:build_integer(0)),
  % (N) when is_integer(N), N >= 0 -> fibonacci(N - 1) + fibonacci(N - 2) 
  FibClauseN =  pt_builder:build_clause(
                  FibVar, 
                  pt_builder:build_and_guard(GuardIsInteger, GuardIsPositive),
                  pt_builder:build_op('+', FibCallN1, FibCallN2)
                ),

  % fibonacci(0) -> 0;
  % fibonacci(1) -> 1;
  % fibonacci(N) when is_integer(N), N >= 0 -> fibonacci(N - 1) + fibonacci(N - 1).
  PT_AST1 = pt_ast:add_function(PT_AST, export, fibonacci, [FibClause0, FibClause1, FibClauseN]),
  PT_AST2 = pt_ast:remove_function(PT_AST1, fib, 1),

  % Generate the output AST
  AST2 = pt_ast:generate(PT_AST2),
  io:format("AST1 = ~p~n", [AST2]),
  AST2.
