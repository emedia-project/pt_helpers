-module(pt_ast).

-include("../include/pt_helpers.hrl").

-export([
         transform/3
         , parse/2
         , generate/1

         , add_function/4
         , add_export/3
        ]).

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
  PT_AST = #pt_ast{functions = AddedFunctions}, 
  Visibility, 
  Name, 
  Clauses
) ->
  case get_arity_(Clauses) of
    {ok, Arity} ->
      NewFun = #pt_fun{
        name = Name,
        arity = Arity,
        clauses = Clauses
      },
      PT_AST1 = PT_AST#pt_ast{
                  functions = AddedFunctions ++ [NewFun]
                 },
      if
        Visibility =:= export ->
          add_export(PT_AST1, Name, Arity);
        true -> PT_AST1
      end;
    _ -> throw(function_clause_add_function)
  end.
get_arity_(Clauses) when is_list(Clauses) ->
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
    end, {ok, -1}, Clauses);
get_arity_(Clauses) ->
  get_arity_([Clauses]).

% @doc
% @end
-spec add_export(pt_ast(), atom(), integer()) -> pt_ast().
add_export(#pt_ast{exports = Exports} = PT_AST, Name, Arity) when is_atom(Name),
                                                                  is_integer(Arity)->
  Export = {Name, Arity},
  case lists:member(Export, Exports) of
    true -> PT_AST;
    false -> PT_AST#pt_ast{exports = [Export|Exports]}
  end.

%% @doc
%% Transform using the given fun
%%
%% Example:
%% <pre>
%% parse_transform(AST, Option) ->
%%   pt_ast:transform(fun do_the_job/1, AST, Option).
%%
%% do_the_job(PT_AST) ->
%%   % Do something with PT_AST
%%   PT_ASTn.
%% </pre>
%% @end
-spec transform(function(), string(), list()) -> ast().
transform(Fun, AST, Options) when is_function(Fun) ->
  PT_AST = parse(AST, Options),
  PT_AST1 = Fun(PT_AST),
  generate(PT_AST1).

%% @doc
%% Parse the given AST and return
%%
%% Example:
%% <pre>
%% parse_transform(AST, Option) ->
%%   PT_AST = pt_ast:parse(AST, Options),
%%   % Do something with PT_AST
%%   pt_ast:generate(PT_AST).
%% </pre>
%% @end
-spec parse(string(), list()) -> pt_ast().
parse(AST, Options) ->
  parse(AST, #pt_ast{options = Options}, 0).

%% @doc
%% Generate the AST
%%
%% Example:
%% <pre>
%% parse_transform(AST, Option) ->
%%   PT_AST = pt_ast:parse(AST, Option),
%%   % Do something with PT_AST
%%   pt_ast:generate(PT_AST).
%% </pre>
%% @end
-spec generate(pt_ast()) -> ast().
generate(#pt_ast{
            % file = File,
            options = Options,
            module = Module,
            compile = Compile,
            exports = Exports,
            attributes = Attributes,
            records = Records,
            types = Types,
            specs = Specs,
            functions = Functions,
            unparsed = Unparsed
           }) ->
  {N0, ASTAttributes} = generate_attributes(Attributes, 4),
  {N1, ASTRecords} = generate_records(Records, N0),
  {N2, ASTTypes} = generate_types(Types, N1),
  {N3, ASTFunctions} = generate_functions(Functions, Specs, N2),
  AST = [
         % {attribute,1,file,File},
         {attribute,1,module,Module},
         {attribute,2,compile,Compile},
         {attribute,3,export,Exports}
        ] ++ ASTAttributes 
  ++ ASTRecords
  ++ ASTTypes
  ++ Unparsed
  ++ ASTFunctions
  ++ [{eof, N3}],
  case pt_utils:is_in(renumber, Options) of
    true -> pt_utils:renumber(AST);
    false -> AST
  end.

generate_types(Types, L) ->
  generate_types(Types, L, []).
generate_types([], L, Acc) ->
  {L, lists:reverse(Acc)};
generate_types([#pt_type{name = Name,
                         def = Definition,
                         data = Data}|Rest], L, Acc) ->
  generate_types(Rest, L + 1, [{attribute,L,type,{Name,Definition,Data}}|Acc]).

generate_attributes(Attributes, L) ->
  maps:fold(fun(Attribute, Value, {N, Attrs}) ->
                {N + 1, [{attribute,N,Attribute,Value}|Attrs]}
            end, {L, []}, Attributes).

generate_functions(Functions, Specs, L) ->
  generate_functions(Functions, Specs, L, []).
generate_functions([], _, L, Acc) ->
  {L, lists:reverse(Acc)};
generate_functions([#pt_fun{name = Name,
                            arity = Arity,
                            clauses = Clauses}|Rest], Specs, L, Acc) ->
  case maps:get({Name, Arity}, Specs, undefined) of
    undefined ->
      generate_functions(Rest, Specs, L + 1, [{function,L,Name,Arity,Clauses}|Acc]);
    Spec ->
      generate_functions(Rest, Specs, L + 2, [{function,L+1,Name,Arity,Clauses},
                                              {attribute,L,specs,{{Name,Arity},Spec}}|Acc])
  end.

generate_records(Records, L) ->
  generate_records(Records, L,[]).
generate_records([], L, Acc) ->
  {L, lists:reverse(Acc)};
generate_records([#pt_record{name = Name,
                             fields = Fields}|Rest], L, Acc) ->
  generate_records(Rest, L + 1, [{attribute,L,record,{Name, Fields}}|Acc]).

parse([], PT_AST, _) ->
  PT_AST;
parse([{attribute, _, file, File}|Rest], PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{file = File}, Index + 1);
parse([{attribute, _, export, Exports}|Rest], #pt_ast{exports = Currents} = PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{exports = Currents ++ Exports}, Index + 1);
parse([{attribute, _, module, Module}|Rest], PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{module = Module}, Index + 1);
parse([{attribute, _, compile, Compile}|Rest], #pt_ast{compile = Currents} = PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{compile = Currents ++ Compile}, Index + 1);
parse([{attribute, _, type, {Name, Definition, Data}}|Rest], #pt_ast{types = Currents} = PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{types = Currents ++ [#pt_type{name = Name,
                                                          def = Definition,
                                                          data = Data}]}, Index + 1);
parse([{attribute, _, record, {Name, Fields}}|Rest], #pt_ast{records = Records} = PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{records = Records ++ [#pt_record{
                                                     name = Name,
                                                     fields = Fields}]},Index + 1);
parse([{attribute, _, spec, {FunAndArity, Spec}}|Rest], #pt_ast{specs = Specs} = PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{specs = maps:put(FunAndArity, Spec, Specs)}, Index + 1);
parse([{attribute, _, Attribute, Data}|Rest], PT_AST, Index) ->
  parse(Rest, merge_attribute_lists(PT_AST, Attribute, Data), Index + 1);
parse([{function, _, Name, Arity, Clauses}|Rest], #pt_ast{functions = Functions} = PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{functions = Functions ++ [#pt_fun{
                                                         name = Name,
                                                         arity = Arity,
                                                         clauses = Clauses}]}, Index + 1);
parse([{eof, N}|Rest], PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{eof = N}, Index + 1);
parse([Def|Rest], #pt_ast{unparsed = CurrentUnparsed} = PT_AST, Index) ->
  parse(Rest, PT_AST#pt_ast{unparsed = CurrentUnparsed ++ [Def]}, Index + 1).

merge_attribute_lists(#pt_ast{attributes = Map} = PT_AST, Key, Values) ->
  Currents = maps:get(Key, Map, []),
  PT_AST#pt_ast{attributes = maps:put(Key, Map, Currents ++ Values)}.

