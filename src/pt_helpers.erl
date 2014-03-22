-module(pt_helpers).

-export([
  parse/2,
  generate/1,

  find_function/3,
  add_function/4,
  add_export/3,
  
  build_atom/1,
  build_integer/1,
  build_float/1,
  build_string/1,
  build_bin/1,
  build_boolean/1,
  build_value/1,

  build_list/1,

  build_var/1,
  build_op/3,

  is_ast/2,
  is_ast/1
]).

-record(pt_ast, {
  ast,
  options,
  main_file,
  compile_options,
  last_line,
  module_name,
  exports = [],
  exports_pos = -1,
  functions = []
}).

-record(pt_fun, {
  index, 
  name, 
  arity
}).

-type pt_ast() :: #pt_ast{}.
-type pt_fun() :: #pt_fun{}.
-type ast() :: tuple() | [tuple()].

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
  io:format("~p~n~p~n", [Options, AST]),
  [{attribute, _, file, {FileName, _}}|NextAST] = AST,
  PT_AST = #pt_ast{
      main_file = FileName,
      ast = AST,
      options = Options
    },
  Index = 1,
  Files = [FileName],
  {_, _, PT_AST1} = lists:foldl(fun parse_definition/2, {Index, Files, PT_AST}, NextAST),
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
generate(PT_AST) ->
  #pt_ast{ast = AST, options = Options} = generate_exports_(PT_AST),
  OutAST = case is_in(renumber, Options) of
    true -> pt_utils:renumber(AST);
    false -> AST
  end,
  io:format("~p~n", [OutAST]),
  OutAST.
generate_exports_(PT_AST = #pt_ast{ast = AST, exports = Exports, exports_pos = Pos}) ->
  {attribute, N, export, _} = lists:nth(Pos + 1, AST),
  PT_AST#pt_ast{ast = change_def(AST, Pos, {attribute, N, export, Exports})}.
  
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
add_function(PT_AST = #pt_ast{ast = AST, functions = Functions}, Visibility, Name, Clauses) ->
  {Arity, ASTClauses} = lists:foldl(fun add_function_clauses_/2, {0, []}, Clauses),
  NewPTFun = #pt_fun{
    index = length(AST),
    name = Name,
    arity = Arity
  },
  ASTFun = {function, 1, Name, Arity, ASTClauses},
  PT_AST1 = PT_AST#pt_ast{ast = add_def(AST, ASTFun), functions = Functions ++ [NewPTFun]},
  if 
    Visibility =:= export -> add_export(PT_AST1, Name, Arity);
    true -> PT_AST1
  end.
add_function_clauses_({Parameters, Clauses, Body}, {_, AST}) ->
  {length(Parameters), AST ++ [{clause, 1, Parameters, Clauses, Body}]}.

%% @doc
%% Declare a new function to export in the AST
%% @end
-spec add_export(pt_ast(), atom(), integer()) -> pt_ast().
add_export(PT_AST = #pt_ast{exports = Exports}, Name, Arity) ->
  PT_AST#pt_ast{exports = Exports ++ [{Name, Arity}]}.

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
-spec build_op(atom(), tuple(), tuple()) -> ast().
build_op(Op, A, B) when is_atom(Op), is_tuple(A), is_tuple(B) ->
  IS_AST = is_ast(A) and is_ast(B),
  if 
    IS_AST -> {op, 1, Op, A, B};
    true -> throw(function_clause)
  end.

%% @doc
%% @end
-spec is_ast(atom(), ast()) -> true | false.
is_ast(Type, AST) ->
  case Type of
    boolean -> element(1, AST) =:= atom andalso (element(3, AST) =:= true orelse element(3, AST) =:= false);
    any -> is_atom(element(1, AST)) and is_integer(element(2, AST));
    T -> element(1, AST) =:= T
  end.

%% @doc
%% @end
-spec is_ast(ast()) -> true | false.
is_ast(AST) -> is_ast(any, AST).

% Private parse

parse_definition({attribute, _, module, ModuleName}, {Index, Files, PT_AST}) ->
  {Index + 1, Files, PT_AST#pt_ast{module_name = ModuleName}};

parse_definition({attribute, _, compile, Options}, {Index, Files, PT_AST}) ->
  {Index + 1, Files, PT_AST#pt_ast{compile_options = Options}};

parse_definition({attribute, _, export, Exports}, {Index, Files, PT_AST = #pt_ast{exports = Exps}}) ->
  {Index + 1, Files, PT_AST#pt_ast{exports = Exps ++ Exports, exports_pos = Index}};

parse_definition({attribute, _, file, {FileName, _}}, {Index, Files, PT_AST}) ->
  {Index + 1, update_files(Files, FileName), PT_AST};

parse_definition({function, _, FunctionName, FunctionArity, _}, {Index, Files, PT_AST = #pt_ast{functions = Functions}}) ->
  NewFun = #pt_fun{index = Index, name = FunctionName, arity = FunctionArity},
  {Index + 1, Files, PT_AST#pt_ast{functions = Functions ++ [NewFun]}};

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

change_def(AST, Index, Element) ->
  lists:sublist(AST, Index) ++ [Element] ++ lists:nthtail(Index + 1, AST).

add_def(AST, Element) ->
  [EOF|Rest] = lists:reverse(AST),
  lists:reverse([EOF] ++ [Element] ++ Rest).

is_in(Element, List) ->
  lists:any(fun(E) -> E =:= Element end, List).

is_string(S) ->
  io_lib:printable_list(S) orelse io_lib:printable_unicode_list(S).
