-module(pt_builder).
-include("../include/pt_helpers.hrl").

-export([
  build_atom/1,
  build_integer/1,
  build_float/1,
  build_string/1,
  build_bin/1,
  build_boolean/1,
  build_list/1,
  build_tuple/1,
  build_value/1,

  build_var/1,
  build_op/3,
  build_match/2,
  build_call/3,
  build_call/2,
  build_clause/2,
  build_clause/3,
  build_guard/1,
  build_and_guard/2,
  build_or_guard/2,

  build_lc/2,
  build_lc_generate/2,

  build_case/2,
  build_if/1,

  build_record/1,
  build_record/2,
  build_record/3,
  build_record_field/2,
  build_get_record_field/2,
  build_get_record_field/3,

  build_fun/1
]).

-spec build_lc(ast(), ast() | [ast()]) -> ast().
build_lc(Var, Generates) when is_list(Generates) ->
  {lc, 1, Var, Generates};
build_lc(Var, Generates) ->
  build_lc(Var, [Generates]).

build_lc_generate(Var, List) ->
  {generate, 1, Var, List}.

-spec build_case(ast(), ast()) -> ast().
build_case(Exp, Clauses) ->
  Clauses1 = if
    is_list(Clauses) -> Clauses;
    true -> [Clauses]
  end,
  _ = pt_utils:if_all_ast(Clauses1, ok),
  case pt_utils:is_ast(Exp) of
    true -> {'case', 1, Exp, Clauses1};
    false -> throw(function_clause_build_case)
  end.

-spec build_if(ast()) -> ast().
build_if(Clauses) ->
  Clauses1 = if
    is_list(Clauses) -> Clauses;
    true -> [Clauses]
  end,
  _ = pt_utils:if_all_ast(Clauses1, ok),
  {'if', 1, Clauses1}.

-spec build_clause(ast(), ast()) -> ast().
build_clause(Vars, Body) -> build_clause(Vars, [], Body).
-spec build_clause(ast(), ast(), ast()) -> ast().
build_clause(Vars, Guards, Body) ->
  Vars1 = if
    is_list(Vars) -> Vars;
    true -> [Vars]
  end,
  _ = pt_utils:if_all_ast(Vars1, ok),
  Body1 = if
    is_list(Body) -> Body;
    true -> [Body]
  end,
  _ = pt_utils:if_all_ast(Body1, ok),
  Guards1 = case list_of_list_of_ast_(Guards) of
    error -> throw(function_clause_build_clause);
    L -> L
  end,
  {clause, 1, Vars1, Guards1, Body1}.

-spec build_guard(ast()) -> ast(). 
build_guard(Ast) when is_tuple(Ast) -> 
  build_guard([[Ast]]);
build_guard(Ast) when is_list(Ast) ->
  ListOfLists = lists:all(fun is_list/1, Ast),
  if
    ListOfLists -> pt_utils:if_all_ast(Ast, Ast);
    true -> build_guard([Ast])
  end.

-spec build_and_guard(ast(), ast()) -> ast().
build_and_guard(Ast1, Ast2) ->
  LLAst1 = list_of_list_of_ast_(Ast1),
  LLAst2 = list_of_list_of_ast_(Ast2),
  if
    LLAst1 =:= error orelse LLAst2 =:= error ->
      throw(function_clause_build_and_guard);
    true ->
      [LastAst1|RestLLAst1] = lists:reverse(LLAst1),
      StartLLAst1 = lists:reverse(RestLLAst1),
      [FirstAst2|EndAst2] = LLAst2,
      StartLLAst1 ++ [LastAst1 ++ FirstAst2] ++ EndAst2
  end.

-spec build_or_guard(ast(), ast()) -> ast().
build_or_guard(Ast1, Ast2) ->
  LLAst1 = list_of_list_of_ast_(Ast1),
  LLAst2 = list_of_list_of_ast_(Ast2),
  if
    LLAst1 =:= error orelse LLAst2 =:= error ->
      throw(function_clause_build_or_guard);
    true ->
      LLAst1 ++ LLAst2
  end.

list_of_list_of_ast_(Ast) ->
  if
    is_list(Ast) -> 
      ListOfLists = lists:all(fun is_list/1, Ast),
      if
        ListOfLists -> 
          ListOfListsOfAst = lists:all(fun(E) ->
                  is_list(E) and lists:all(fun(F) ->
                        not is_list(F) and pt_utils:is_ast(F)
                    end, E)
              end, Ast),
          if
            ListOfListsOfAst -> Ast;
            true -> error
          end;
        true ->
          SomeList = lists:any(fun is_list/1, Ast),
          if
            SomeList -> error;
            true -> list_of_list_of_ast_(lists:map(fun(E) -> [E] end, Ast))
          end
      end;
    true -> list_of_list_of_ast_([Ast])
  end.

%% @doc
%% ASTify an atom
%% @end
-spec build_atom(atom()) -> ast().
build_atom(A) when is_atom(A) ->
  {atom, 1, A}.

%% @doc
%% ASTify an integer
%% @end
-spec build_integer(integer()) -> ast().
build_integer(I) when is_integer(I) ->
  {integer, 1, I}.

%% @doc
%% ASTify a float
%% @end
-spec build_float(float()) -> ast().
build_float(F) when is_float(F) ->
  {float, 1, F}.

%% @doc
%% ASTify a string
%% @end
-spec build_string(string()) -> ast().
build_string(S) when is_list(S) ->
  build_list(S).

%%% @doc
%%% ASTify a list
%%% @end
-spec build_list(list()) -> ast().
build_list(L) when is_list(L) ->
  IsString = pt_utils:is_string(L),
  if 
    IsString =:= true -> {string, 1, L};
    true -> build_list_(L)
  end.
build_list_([]) -> {nil,1};
build_list_(L) ->
  [T|H] = lists:reverse(L),
  E = {cons, 1, build_value(T), {nil, 1}},
  do_build_list_(H, E).
do_build_list_([], E) ->
  E;
do_build_list_([T|H], R) ->
  E = {cons, 1, build_value(T), R},
  do_build_list_(H, E).

%% @doc
%% ASTify a tuple
%% @end
-spec build_tuple(tuple()) -> ast().
build_tuple(T) when is_tuple(T) ->
  TupleContent = lists:map(fun build_value/1, tuple_to_list(T)),
  {tuple, 1, TupleContent}.

%% @doc
%% ASTify a bin
%% @end
build_bin(S) when is_bitstring(S) ->
  LS = binary_to_list(S),
  IsString = pt_utils:is_string(LS),
  if
    IsString =:= true ->
      {bin, 1, [{bin_element, 1, {string, 1, LS}, default, default}]};
    true ->
      BinElements = lists:foldl(fun(E, Acc) ->
            IsString1 = pt_utils:is_string(E),
            Acc ++ if
              IsString1 =:= true -> 
                [{bin_element, 1, {string, 1, E}, default, default}];
              true ->
                [{bin_element, 1, {integer, 1, E}, default, default}]
            end
        end, [], LS),
      {bin, 1, BinElements}
  end.

%% @doc
%% ASTify a boolean
%% @end
build_boolean(B) when is_boolean(B) ->
  build_atom(B).

%% @doc
%%% ASTify the given value
%% @end
build_value(X) when is_atom(X) ->
  build_atom(X);
build_value(X) when is_integer(X) ->
  build_integer(X);
build_value(X) when is_float(X) ->
  build_float(X);
build_value(X) when is_bitstring(X) ->
  build_bin(X);
build_value(X) when is_boolean(X) ->
  build_boolean(X);
build_value(X) when is_list(X) ->
  build_list(X);
build_value(X) when is_tuple(X) ->
  case pt_utils:is_ast(X) of
    true -> X;
    false -> build_tuple(X)
  end.

%% @doc
%% ASTify a record
%%
%% Example:
%% <pre>
%% pt_builder:build_record(rec) % => #rec{}
%% </pre>
%% @end
build_record(Name) when is_atom(Name) ->
  build_record(Name, []).
%% @doc
%% ASTify a record
%%
%% Example:
%% <pre>
%% F1 = pt_builder:build_record_field(field1, pt_builder:build_value(12)),
%% F2 = pt_builder:build_record_field(field2, pt_builder:build_var('X'),
%% pt_builder:build_record(rec, [F1, F2]) % => #rec{field1 = 12, field2 = X}
%% </pre>
%% @end
build_record(Name, Fields) when is_atom(Name), is_list(Fields) ->
  {record, 1, Name, Fields};
%% @doc
%% ASTify a record
%%
%% Example:
%% <pre>
%% pt_builder:build_record('X', rec) % => X#rec{}
%% </pre>
%% @end
build_record(Record, Name) when is_atom(Name) ->
  build_record(Record, Name, []).
%% @doc
%% ASTify a record
%%
%% Example:
%% <pre>
%% F1 = pt_builder:build_record_field(field1, pt_builder:build_value(12)),
%% F2 = pt_builder:build_record_field(field2, pt_builder:build_var('X'),
%% pt_builder:build_record('R', rec, [F1, F2]) % => R#rec{field1 = 12, field2 = X}
%% </pre>
%% @end
build_record(Record, Name, Fields) when is_atom(Name), is_list(Fields) ->
  {record, 1, 
   build_var(Record),
   Name,
   Fields}.

%% @doc
%% ASTify a variable
%% @end
-spec build_var(atom()) -> ast().
build_var(A) when is_atom(A) ->
  {var, 1, A}.

%% @doc
%% ASTify an operator
%% @end
-spec build_op(atom(), ast(), ast()) -> ast().
build_op(Op, A, B) when is_atom(Op), is_tuple(A), is_tuple(B) ->
  pt_utils:if_all_ast([A, B], {op, 1, Op, A, B}).

%% @doc
%% ASTify a match (=)
%% @end
-spec build_match(ast(), ast()) -> ast().
build_match(A, B) when is_tuple(A), is_tuple(B) ->
  pt_utils:if_all_ast([A, B], {match, 1, A, B}).

%% @doc
%% ASTify a function call
%%
%% Example:
%% <pre>
%% A = build_atom(atom),
%% B = build_var('Var'),
%% build_call(module, function, [A, B]) % == module:function(atom, Var)
%% </pre>
%% @end
-spec build_call(atom() | ast(), atom() | ast(), ast()) -> ast().
build_call(Module, Function, Parameters) when is_list(Parameters) ->
  Module1 = case pt_utils:is_ast(Module) of
    true -> Module;
    false -> build_atom(Module)
  end,
  Function1 = case pt_utils:is_ast(Function) of
    true -> Function;
    false -> build_atom(Function)
  end,
  {call, 1, 
    {remote, 1, 
      Module1,
      Function1}, 
    Parameters};
build_call(Module, Function, Parameters) when is_tuple(Parameters) ->
  build_call(Module, Function, [Parameters]).

%% @doc
%% ASTify a function call
%%
%% Example:
%% <pre>
%% A = build_atom(atom),
%% B = build_var('Var'),
%% build_call(function, [A, B]) % == function(atom, Var)
%% </pre>
%% @end
-spec build_call(atom(), ast()) -> ast().
build_call(Function, Parameters) when is_atom(Function), is_list(Parameters) ->
  {call, 1, build_atom(Function), Parameters};
build_call(Function, Parameters) when is_atom(Function), is_tuple(Parameters) ->
  build_call(Function, [Parameters]).

%% @doc
%% ASTify a record_field
%%
%% Example:
%% <pre>
%% build_get_record_field('R', record, field) % == R#record.field
%% </pre>
%% @end
-spec build_get_record_field(atom() | ast(), atom(), atom()) -> ast().
build_get_record_field(RecordVar, RecordName, Field) when is_atom(RecordName), is_atom(Field) ->
  if
    is_atom(RecordVar) -> 
      {record_field, 1, 
        build_var(RecordVar),
        RecordName, 
        build_atom(Field)};
    is_tuple(RecordVar) ->
      case pt_utils:is_ast(RecordVar) of
        true ->
          {record_field, 1, 
            RecordVar,
            RecordName, 
            build_atom(Field)};
        false ->
          throw(function_clause_build_get_record_field_0)
      end;
    true ->
      throw(function_clause_build_get_record_field_1)
  end.

%% @doc
%% ASTify a record_field
%%
%% Example:
%% <pre>
%% build_get_record_field(record, field) % == record.field
%% </pre>
%% @end
-spec build_get_record_field(atom(), atom()) -> ast().
build_get_record_field(RecordName, Field) when is_atom(RecordName), is_atom(Field) ->
  {record_index, 1,
    RecordName,
    build_atom(Field)}.

%% @doc
%% ASTify a record_field
%%
%% Example:
%% <pre>
%% build_record_field(record, field) % == {record = field}
%% </pre>
%% @end
-spec build_record_field(atom(), ast()) -> ast().
build_record_field(Field, Value) when is_atom(Field) ->
  {record_field, 1, build_atom(Field), Value}.

%% @doc
%% ASTify a fun
%% @end
build_fun(Clauses) when is_list(Clauses) ->
  {'fun', 1, {clauses, Clauses}};
build_fun(Clause) when is_tuple(Clause) ->
  build_fun([Clause]).

