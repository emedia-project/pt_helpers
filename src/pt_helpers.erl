-module(pt_helpers).

-include("../include/pt_helpers_defs.hrl").

-export([
  parse/2,
  generate/1,

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
  build_value/1,

  build_var/1,
  build_op/3,
  build_match/2,
  build_call/3,
  build_call/2,

  is_ast/2,
  is_ast/1,
  get_ast_type/1,

  fields/1
]).

%% @doc
%% Parse the given AST and return
%%
%% Example:
%% <pre>
%% parse_transform(AST, _Option) ->
%%   PT_AST = pt_helpers:parse(AST),
%%   % Do something with PT_AST
%%   pt_helpers(PT_AST).
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
%% parse_transform(AST, _Option) ->
%%   PT_AST = pt_helpers:parse(AST),
%%   % Do something with PT_AST
%%   pt_helpers(PT_AST).
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
%% @end
-spec add_function(pt_ast(), export | not_export, atom(), list()) -> pt_ast().
add_function(
  PT_AST = #pt_ast{added_functions = AddedFunctions}, 
  Visibility, 
  Name, 
  Clauses
) ->
  {Arity, ASTClauses} = lists:foldl(
    fun add_function_clauses_/2, 
    {0, []}, 
    Clauses
  ),
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
  }.
add_function_clauses_({Parameters, Clauses, Body}, {_, AST}) ->
  {
    length(Parameters), 
    AST ++ [{clause, 1, Parameters, Clauses, Body}]
  }.

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
  build_list(X).


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
  if_all_ast_([A, B], {op, 1, Op, A, B}).

%% @doc
%% ASTify a match (=)
%% @end
-spec build_match(ast(), ast()) -> ast().
build_match(A, B) when is_tuple(A), is_tuple(B) ->
  if_all_ast_([A, B], {match, 1, A, B}).

if_all_ast_(List, Result) ->
  IS_AST = lists:all(fun(E) ->
        is_ast(E)
    end, List),
  if 
    IS_AST -> Result;
    true -> throw(function_clause)
  end.

%% @doc
%% ASTify a function call
%% @end
build_call(Module, Function, Parameters) when is_atom(Module), is_atom(Function), is_list(Parameters) ->
  {call, 1, {remote, 1, {atom, 1, Module}, {atom, 1, Function}}, Parameters}.

%% @doc
%% ASTify a function call
%% @end
build_call(Function, Parameters) when is_atom(Function), is_list(Parameters) ->
  {call, 1, {atom, 1, Function}, Parameters}.

%% @doc
%% @end
-spec is_ast(atom(), ast()) -> true | false.
is_ast(Type, AST) ->
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
  end.
%% @doc
%% @end
-spec is_ast(ast()) -> true | false.
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
