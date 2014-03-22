-module(pt_helpers_tests).

-include_lib("eunit/include/eunit.hrl").
-define(assertTrue(X), ?assert(true =:= (X))).
-define(assertFalse(X), ?assert(false =:= (X))).

pt_helpers_test_() ->
  {setup,
    fun setup/0, fun teardown/1,
    [
      ?_test(build_atom()),
      ?_test(build_var()),
      ?_test(build_op())
    ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

build_atom() -> 
  ?assertMatch({atom, _, a}, pt_helpers:build_atom(a)),
  ?assertMatch({atom, _, 'A'}, pt_helpers:build_atom('A')),
  ?assertError(function_clause, pt_helpers:build_atom(1)),
  ?assertTrue(pt_helpers:is_ast(atom, pt_helpers:build_atom('A'))),
  ?assertFalse(pt_helpers:is_ast(integer, pt_helpers:build_atom('A'))),
  ?assertFalse(pt_helpers:is_ast(boolean, pt_helpers:build_atom('A'))),
  ?assertFalse(pt_helpers:is_ast(bin, pt_helpers:build_atom('A'))).

build_var() ->
  ?assertMatch({var, _, 'A'}, pt_helpers:build_var('A')),
  ?assertError(function_clause, pt_helpers:build_var(1)).

build_op() ->
  A = pt_helpers:build_var('A'),
  B = pt_helpers:build_var('B'),
  ?assertMatch({op, _, '*', _, _}, pt_helpers:build_op('*', A, B)),
  ?assertError(function_clause, pt_helpers:build_op('*', a, b)).
