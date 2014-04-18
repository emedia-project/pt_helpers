-module(pt_helpers).

-include("../include/pt_helpers_defs.hrl").

-export([
  parse/2,
  generate/1,
  transform/3,

  module_name/1,
  find_function/3,
  add_function/4,
  add_export/3,

  add_record/3,

  index/2,
  
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

  build_record_field/2,
  build_record_field/3,

  is_ast/2,
  is_ast/1,
  get_ast_type/1,

  fields/1
]).

%% @doc
%% Return the name of the module
%% @end
-spec module_name(pt_ast()) -> string().
module_name(#pt_ast{module_name = ModuleName}) ->
  ModuleName.

-spec build_clause(ast(), ast()) -> ast().
build_clause(Vars, Body) -> build_clause(Vars, [], Body).
-spec build_clause(ast(), ast(), ast()) -> ast().
build_clause(Vars, Guards, Body) ->
  Vars1 = if
    is_list(Vars) -> Vars;
    true -> [Vars]
  end,
  _ = if_all_ast(Vars1, ok),
  Body1 = if
    is_list(Body) -> Body;
    true -> [Body]
  end,
  _ = if_all_ast(Body1, ok),
  GuardsIsAst = lists:all(fun(E) ->
        is_list(E) and lists:all(fun(F) ->
              is_ast(F)
          end, E)
    end, Guards),
  Guards1 = if
    GuardsIsAst -> Guards;
    true -> throw(function_clause)
  end,
  {clause, 1, Vars1, Guards1, Body1}.

-spec build_guard(ast()) -> ast(). 
build_guard(Ast) when is_tuple(Ast) -> 
  build_guard([[Ast]]);
build_guard(Ast) when is_list(Ast) ->
  ListOfLists = lists:all(fun is_list/1, Ast),
  if
    ListOfLists -> if_all_ast(Ast, Ast);
    true -> build_guard([Ast])
  end.

-spec build_and_guard(ast(), ast()) -> ast().
build_and_guard(Ast1, Ast2) ->
  LLAst1 = list_of_list_of_ast_(Ast1),
  LLAst2 = list_of_list_of_ast_(Ast2),
  if
    LLAst1 =:= error orelse LLAst2 =:= error ->
      throw(function_clause);
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
      throw(function_clause);
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
                        not is_list(F) and is_ast(F)
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
%% Parse the given AST and return
%%
%% Example:
%% <pre>
%% parse_transform(AST, Option) ->
%%   PT_AST = pt_helpers:parse(AST, Options),
%%   % Do something with PT_AST
%%   pt_helpers:generate(PT_AST).
%% </pre>
%% @end
-spec parse(string(), list()) -> pt_ast().
parse(AST, Options) ->
  [{attribute, _, file, {FileName, _}}|NextAST] = AST,
  PT_AST = #pt_ast{
      main_file = FileName,
      ast = AST,
      options = Options
    },
  Index = 1,
  Files = [FileName],
  {_, _, PT_AST1} = lists:foldl(
    fun parse_definition/2, 
    {Index, Files, PT_AST}, 
    NextAST
  ),
  PT_AST1.

%% @doc
%% Generate the AST
%%
%% Example:
%% <pre>
%% parse_transform(AST, Option) ->
%%   PT_AST = pt_helpers:parse(AST, Option),
%%   % Do something with PT_AST
%%   pt_helpers:generate(PT_AST).
%% </pre>
%% @end
-spec generate(pt_ast()) -> ast().
generate(#pt_ast{
    ast = AST,
    exports_pos = ExportPos, 
    function_pos = FunctionPos,
    options = Options,
    added_functions = AddedFunctions,
    added_records = AddedRecords
  }) ->
  {AstToExport, AstTypes, AstFunctions, AstEof} = cut_ast_(
    AST,
    ExportPos, 
    FunctionPos
  ),
  AST1 = 
    AstToExport ++ 
    generate_exports_(AddedFunctions) ++
    AstTypes ++
    generate_records_(AddedRecords) ++
    AstFunctions ++
    generate_functions_(AddedFunctions) ++
    AstEof,
  %% Option +renumber
  OutAST = case is_in(renumber, Options) of
    true -> pt_utils:renumber(AST1);
    false -> AST1
  end,
  OutAST.

%% @doc
%% TRansform using the given fun
%%
%% Example:
%% <pre>
%% parse_transform(AST, Option) ->
%%   pt_helpers:transform(fun do_the_job/1, AST, Option).
%%
%% do_the_job(PT_AST) ->
%%   % Do something with PT_AST
%%   PT_ASTn.
%% </pre>
%% @end
-spec transform(function(), string(), list()) -> ast().
transform(Fun, AST, Options) ->
  PT_AST = parse(AST, Options),
  PT_AST1 = Fun(PT_AST),
  generate(PT_AST1).

cut_ast_(AST, -1, -1) ->
  {
    lists:sublist(AST, 1, length(AST) - 1), 
    [], 
    [],
    [lists:nth(length(AST), AST)]
  };
cut_ast_(AST, -1, FunctionPos) ->
  {
    lists:sublist(AST, 1, FunctionPos),
    [],
    lists:sublist(AST, FunctionPos + 1, length(AST) - FunctionPos - 1),
    [lists:nth(length(AST), AST)]
  };
cut_ast_(AST, ExportPos, -1) ->
  {
    lists:sublist(AST, 1, ExportPos + 1),
    lists:sublist(AST, ExportPos + 2, length(AST) - ExportPos - 2),
    [],
    [lists:nth(length(AST), AST)]
  };
cut_ast_(AST, ExportPos, FunctionPos) ->
  {
    lists:sublist(AST, 1, ExportPos + 1),
    lists:sublist(AST, ExportPos + 2, FunctionPos - ExportPos - 1),
    lists:sublist(AST, FunctionPos + 1, length(AST) - FunctionPos - 1),
    [lists:nth(length(AST), AST)]
  }.

generate_exports_(AddedFunctions) ->
  Exports = lists:foldl(
    fun(#pt_fun{name = Name, arity = Arity}, Ast) ->
        Ast ++ [{Name, Arity}]
    end, [], AddedFunctions),
  if
    length(Exports) > 0 -> [{attribute, 1, export, Exports}];
    true -> []
  end.

generate_functions_(AddedFunctions) ->
  lists:foldl(
    fun(#pt_fun{ast = AstFunction}, Ast) ->
        Ast ++ [AstFunction]
    end, [], AddedFunctions).

generate_records_(AddedRecords) ->
  lists:foldl(
    fun(#pt_record{
          ast = AstRecord,
          ast_type = AstRecordType,
          has_type = HasType}, Ast) ->
        Ast ++ [AstRecord] ++ if
          HasType =:= true -> [AstRecordType];
          true -> []
        end
    end, [], AddedRecords).
  
%% @doc
%% Find the function with name <tt>Name</tt> and arity <tt>Arity</tt> in the parsed AST
%%
%% Example:
%% <pre>
%% parse_transform(AST, _Option) ->
%%   PT_AST = pt_helpers:parse(AST),
%%   PT_FUN = pt_helpers:find_function(PT_AST, my_function, 2).
%%   % Do something with PT_FUN
%%   pt_helpers(PT_AST).
%% </pre>
%% @end
-spec find_function(pt_ast(), atom(), integer()) -> {ok, pt_fun()} | {error, not_found}.
find_function(#pt_ast{functions = Functions}, Name, Arity) ->
  find_function_(Functions, Name, Arity).
find_function_([], _, _) ->
  {error, not_found};
find_function_([Fun = #pt_fun{name = Name, arity = Arity}|_], Name, Arity) ->
  {ok, Fun};
find_function_([_|Rest], Name, Arity) ->
  find_function_(Rest, Name, Arity).

%% @doc
%% Add a function to the AST
%%
%% Example:
%% <pre>
%% A = pt_helpers:build_var('A'),
%% B = pt_helpers:build_var('B'),
%% AIsNumber = pt_helpers:build_call(is_number, A),
%% BIsNumber = pt_helpers:build_call(is_number, B),
%% Guards = pt_helpers:build_and_guard(AIsNumber, BIsNumber),
%% Body = pt_helpers:build_op('*', A, B),
%% Clauses = pt_helpers:build_clause([A, B], Guards, Body),
%% PT_AST1 = pt_helpers:add_function(PT_AST, export, my_function, Clauses)
%% % => my_function(A, B) when is_number(A), is_number(B) -> A * B
%% </pre>
%% @end
-spec add_function(pt_ast(), export | not_export, atom(), tuple() | list()) -> pt_ast().
add_function(
  PT_AST = #pt_ast{added_functions = AddedFunctions}, 
  Visibility, 
  Name, 
  Clauses
) ->
  ArityAndClause = case is_list(Clauses) of
    true -> {S, A} = get_arity_(Clauses), {S, A, Clauses};
    false -> {S, A} = get_arity_([Clauses]), {S, A, [Clauses]}
  end,
  case ArityAndClause of
    {ok, Arity, ASTClauses} ->
      ASTFun = {function, 1, Name, Arity, ASTClauses},
      NewFun = #pt_fun{
        name = Name,
        visibility = Visibility,
        arity = Arity,
        clauses = Clauses,
        ast = ASTFun
      },
      PT_AST#pt_ast{
        added_functions = AddedFunctions ++ [NewFun]
      };
    _ -> throw(function_clause)
  end.
get_arity_(Clauses) ->
  lists:foldl(fun(Clause, {Status, Arity}) ->
        if
          Status =:= ok -> 
            case Clause of
              {clause, _, Parameters, _, _} -> 
                if
                  Arity =:= -1 orelse Arity =:= length(Parameters) -> {Status, length(Parameters)};
                  true -> {error, Arity}
                end;
              _ -> {error, Arity}
            end;
          true -> {Status, Arity}
        end
    end, {ok, -1}, Clauses).

%% @doc
%% Declare a new function to export in the AST
%% @end
-spec add_export(pt_ast(), atom(), integer()) -> pt_ast().
add_export(PT_AST = #pt_ast{exports = Exports}, Name, Arity) ->
  PT_AST#pt_ast{exports = Exports ++ [{Name, Arity}]}.

%% @doc
%% Declare a new record to export in the AST
%% @end
-spec add_record(pt_ast(), atom(), list()) -> pt_ast().
add_record(PT_AST = #pt_ast{added_records = AddedRecs}, Name, Attributes) ->
  {Attrs, AttrTypes, HasType} = add_record_fields_(Attributes, [], [], false),
  AstRecord = {attribute, 1, record, {Name, Attrs}},
  AstType = {attribute, 1, type, {{record, Name}, AttrTypes}},
  Record = #pt_record{
    name = Name,
    fields = Attributes,
    ast = AstRecord,
    ast_type = AstType,
    has_type = HasType
  },
  PT_AST#pt_ast{added_records = AddedRecs ++ [Record]}.
add_record_fields_([], Result, ResultType, HasTypes) -> 
  {Result, ResultType, HasTypes};
add_record_fields_([{Attr, Types}|Attributes], Result, ResultType, HasTypes) when is_list(Types) ->
  {NewResultType, NewHasType} = if 
    length(Types) > 0 -> 
      {ResultType ++ [
          {typed_record_field,
            {record_field, 1, build_atom(Attr)},
            {type, 1, union, 
              [{atom, 1, undefined}] ++ lists:map(fun(E) ->
                    {type, 1, E, []}
                end, Types)
            }}
        ], true};
    true ->
      {ResultType ++ [
          {record_field, 1, build_atom(Attr)}
        ], HasTypes}
  end,
  add_record_fields_(
    Attributes, 
    Result ++ [{record_field, 1, build_atom(Attr)}],
    NewResultType,
    NewHasType
  );
add_record_fields_([{Attr, Type}|Attributes], Result, ResultType, _HasTypes) when is_atom(Type) ->
  add_record_fields_(
    Attributes, 
    Result ++ [{record_field, 1, build_atom(Attr)}],
    ResultType ++ [
      {typed_record_field,
        {record_field, 1, build_atom(Attr)},
        {type, 1, union, [{atom, 1, undefined}, {type, 1, Type, []}]}}
    ],
    true
  );
add_record_fields_([Attr|Attributes], Result, ResultType, HasTypes) when is_atom(Attr) ->
  add_record_fields_(
    Attributes, 
    Result ++ [{record_field, 1, build_atom(Attr)}],
    ResultType ++ [{record_field, 1, build_atom(Attr)}],
    HasTypes
  ).

%% @doc
%% Return AST at index
%% @end
-spec index(pt_ast(), integer()) -> ast().
index(#pt_ast{ast = AST}, I) ->
  if
    length(AST) =< I -> {ok, lists:nth(I, AST)};
    true -> {error, not_found}
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
  IsString = is_string(L),
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
  IsString = is_string(LS),
  if
    IsString =:= true ->
      {bin, 1, [{bin_element, 1, {string, 1, LS}, default, default}]};
    true ->
      BinElements = lists:foldl(fun(E, Acc) ->
            IsString1 = is_string(E),
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
  build_tuple(X).

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
  if_all_ast([A, B], {op, 1, Op, A, B}).

%% @doc
%% ASTify a match (=)
%% @end
-spec build_match(ast(), ast()) -> ast().
build_match(A, B) when is_tuple(A), is_tuple(B) ->
  if_all_ast([A, B], {match, 1, A, B}).

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
-spec build_call(atom(), atom(), ast()) -> ast().
build_call(Module, Function, Parameters) when is_atom(Module), is_atom(Function), is_list(Parameters) ->
  {call, 1, 
    {remote, 1, 
      build_atom(Module),
      build_atom(Function)}, 
    Parameters};
build_call(Module, Function, Parameters) when is_atom(Module), is_atom(Function), is_tuple(Parameters) ->
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
%% build_record_field('R', record, field) % == R#record.field
%% </pre>
%% @end
-spec build_record_field(atom() | ast(), atom(), atom()) -> ast().
build_record_field(RecordVar, RecordName, Field) when is_atom(RecordName), is_atom(Field) ->
  if
    is_atom(RecordVar) -> 
      {record_field, 1, 
        build_var(RecordVar),
        RecordName, 
        build_atom(Field)};
    is_tuple(RecordVar) ->
      case is_ast(RecordVar) of
        true ->
          {record_field, 1, 
            RecordVar,
            RecordName, 
            build_atom(Field)};
        false ->
          throw(function_clause)
      end;
    true ->
      throw(function_clause)
  end.

%% @doc
%% ASTify a record_field
%%
%% Example:
%% <pre>
%% build_record_field(record, field) % == record.field
%% </pre>
%% @end
-spec build_record_field(atom(), atom()) -> ast().
build_record_field(RecordName, Field) when is_atom(RecordName), is_atom(Field) ->
  {record_index, 1,
    RecordName,
    build_atom(Field)}.

%% @doc
%% @end
-spec is_ast(atom(), ast() | [ast()]) -> true | false.
is_ast(Type, AST) when is_tuple(AST) ->
  case Type of
    boolean -> element(1, AST) =:= atom andalso (element(3, AST) =:= true orelse element(3, AST) =:= false);
    any -> is_atom(element(1, AST)) and is_integer(element(2, AST));
    T -> 
      First = element(1, AST),
      if
        First =:= T -> true;
        First =:= attribute -> element(3, AST) =:= Type;
        true -> false
      end
  end;
is_ast(Type, AST) when is_list(AST) ->
  lists:all(fun(E) ->
        is_ast(Type, E)
    end, AST).

%% @doc
%% @end
-spec is_ast(ast() | [ast()]) -> true | false.
is_ast(AST) -> is_ast(any, AST).

%% @doc
%% Return the type of the given AST
%% @end
-spec get_ast_type(ast()) -> {ok, atom()} | {error, wrong_ast}.
get_ast_type(E) when is_tuple(E) ->
  IsAst = is_ast(E),
  if
    IsAst =:= true -> 
      First = element(1, E),
      if
        First =:= attribute -> {ok, element(3, E)};
        true -> First
      end;
    true -> {error, wrong_ast}
  end.

%% @doc
%% Return the list of all availables fields
%% @end
-spec fields(ast()) -> list().
fields(#pt_ast{fields = Fields}) -> 
  lists:map(fun(#pt_field{data = Data}) ->
        Data
    end, Fields).

% Private parse

parse_definition({attribute, _, module, ModuleName}, {Index, Files, PT_AST}) ->
  {Index + 1, Files, PT_AST#pt_ast{module_name = ModuleName}};

parse_definition({attribute, _, compile, Options}, {Index, Files, PT_AST}) ->
  {Index + 1, Files, PT_AST#pt_ast{compile_options = Options}};

parse_definition({attribute, _, export, Exports}, {Index, Files, PT_AST = #pt_ast{exports = Exps}}) ->
  {Index + 1, Files, PT_AST#pt_ast{exports = Exps ++ Exports, exports_pos = Index}};

parse_definition({attribute, _, file, {FileName, _}}, {Index, Files, PT_AST}) ->
  {Index + 1, update_files(Files, FileName), PT_AST};

parse_definition({function, _, FunctionName, FunctionArity, _}, {Index, Files, PT_AST = #pt_ast{functions = Functions, function_pos = FFP}}) ->
  NewFFP = if 
    FFP =:= -1 -> Index;
    true -> FFP
  end,
  {
    Index + 1, 
    Files, 
    PT_AST#pt_ast{
      functions = Functions ++ [
        #pt_fun{
          index = Index, 
          name = FunctionName, 
          arity = FunctionArity
        }
      ], 
      function_pos = NewFFP
    }
  };

parse_definition({attribute, _, field, Field}, {Index, Files, PT_AST = #pt_ast{fields = Fields}}) ->
  NewField = #pt_field{index = Index, data = Field},
  {Index + 1, Files, PT_AST#pt_ast{fields = Fields ++ [NewField]}};

parse_definition({eof, N}, {Index, Files, PT_AST}) ->
  {Index + 1, Files, PT_AST#pt_ast{last_line = N}};

parse_definition(_Def, {Index, Files, PT_AST}) ->
  {Index + 1, Files, PT_AST}.

% Private utils

update_files(Files, File) ->
  NB = length(Files),
  if 
    NB =:= 1 -> 
      [File] ++ Files;
    true -> 
      [_ActualFile, CurrentFile|Rest] = Files,
      if 
        CurrentFile =:= File -> 
          [File] ++ Rest;
        true -> 
          [File] ++ Files
      end
  end.

% change_def(AST, Index, Element) ->
%   lists:sublist(AST, Index) ++ [Element] ++ lists:nthtail(Index + 1, AST).
% 
% add_def(AST, Element) ->
%   [EOF|Rest] = lists:reverse(AST),
%   lists:reverse([EOF] ++ [Element] ++ Rest).
% 
% add_def(AST, Index, Element) ->
%   if
%     Index =< 0 -> add_def(AST, Element);
%     true -> lists:sublist(AST, Index) ++ [Element] ++ lists:nthtail(Index, AST)
%   end.

is_in(Element, List) ->
  lists:any(fun(E) -> E =:= Element end, List).

is_string(S) ->
  io_lib:printable_list(S) orelse io_lib:printable_unicode_list(S).

if_all_ast([], Result) -> Result;
if_all_ast(List, Result) ->
  IS_AST = is_ast(List),
  if 
    IS_AST -> Result;
    true -> throw(function_clause)
  end.
