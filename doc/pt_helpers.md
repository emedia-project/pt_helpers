

# Module pt_helpers #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_export-3">add_export/3</a></td><td>
Declare a new function to export in the AST.</td></tr><tr><td valign="top"><a href="#add_function-4">add_function/4</a></td><td>
Add a function to the AST.</td></tr><tr><td valign="top"><a href="#add_record-3">add_record/3</a></td><td>
Declare a new record to export in the AST.</td></tr><tr><td valign="top"><a href="#build_and_guard-2">build_and_guard/2</a></td><td></td></tr><tr><td valign="top"><a href="#build_atom-1">build_atom/1</a></td><td>
ASTify an atom.</td></tr><tr><td valign="top"><a href="#build_bin-1">build_bin/1</a></td><td>
ASTify a bin.</td></tr><tr><td valign="top"><a href="#build_boolean-1">build_boolean/1</a></td><td>
ASTify a boolean.</td></tr><tr><td valign="top"><a href="#build_call-2">build_call/2</a></td><td>
ASTify a function call.</td></tr><tr><td valign="top"><a href="#build_call-3">build_call/3</a></td><td>
ASTify a function call.</td></tr><tr><td valign="top"><a href="#build_case-2">build_case/2</a></td><td></td></tr><tr><td valign="top"><a href="#build_clause-2">build_clause/2</a></td><td></td></tr><tr><td valign="top"><a href="#build_clause-3">build_clause/3</a></td><td></td></tr><tr><td valign="top"><a href="#build_float-1">build_float/1</a></td><td>
ASTify a float.</td></tr><tr><td valign="top"><a href="#build_fun-1">build_fun/1</a></td><td>
ASTify a fun.</td></tr><tr><td valign="top"><a href="#build_get_record_field-2">build_get_record_field/2</a></td><td>
ASTify a record_field.</td></tr><tr><td valign="top"><a href="#build_get_record_field-3">build_get_record_field/3</a></td><td>
ASTify a record_field.</td></tr><tr><td valign="top"><a href="#build_guard-1">build_guard/1</a></td><td></td></tr><tr><td valign="top"><a href="#build_if-1">build_if/1</a></td><td></td></tr><tr><td valign="top"><a href="#build_integer-1">build_integer/1</a></td><td>
ASTify an integer.</td></tr><tr><td valign="top"><a href="#build_lc-2">build_lc/2</a></td><td></td></tr><tr><td valign="top"><a href="#build_lc_generate-2">build_lc_generate/2</a></td><td></td></tr><tr><td valign="top"><a href="#build_list-1">build_list/1</a></td><td>
ASTify a list.</td></tr><tr><td valign="top"><a href="#build_match-2">build_match/2</a></td><td>
ASTify a match (=).</td></tr><tr><td valign="top"><a href="#build_op-3">build_op/3</a></td><td>
ASTify an operator.</td></tr><tr><td valign="top"><a href="#build_or_guard-2">build_or_guard/2</a></td><td></td></tr><tr><td valign="top"><a href="#build_record-1">build_record/1</a></td><td>
ASTify a record.</td></tr><tr><td valign="top"><a href="#build_record-2">build_record/2</a></td><td>
ASTify a record.</td></tr><tr><td valign="top"><a href="#build_record-3">build_record/3</a></td><td>
ASTify a record.</td></tr><tr><td valign="top"><a href="#build_record_field-2">build_record_field/2</a></td><td>
ASTify a record_field.</td></tr><tr><td valign="top"><a href="#build_string-1">build_string/1</a></td><td>
ASTify a string.</td></tr><tr><td valign="top"><a href="#build_tuple-1">build_tuple/1</a></td><td>
ASTify a tuple.</td></tr><tr><td valign="top"><a href="#build_value-1">build_value/1</a></td><td>
ASTify the given value.</td></tr><tr><td valign="top"><a href="#build_var-1">build_var/1</a></td><td>
ASTify a variable.</td></tr><tr><td valign="top"><a href="#directive-2">directive/2</a></td><td>
Return the value for a directive Name.</td></tr><tr><td valign="top"><a href="#fields-1">fields/1</a></td><td>
Return the list of all availables fields.</td></tr><tr><td valign="top"><a href="#find_function-3">find_function/3</a></td><td>
Find the function with name <tt>Name</tt> and arity <tt>Arity</tt> in the parsed AST.</td></tr><tr><td valign="top"><a href="#find_functions-2">find_functions/2</a></td><td></td></tr><tr><td valign="top"><a href="#generate-1">generate/1</a></td><td>
Generate the AST.</td></tr><tr><td valign="top"><a href="#get_ast_type-1">get_ast_type/1</a></td><td>
Return the type of the given AST.</td></tr><tr><td valign="top"><a href="#index-2">index/2</a></td><td>
Return AST at index.</td></tr><tr><td valign="top"><a href="#is_ast-1">is_ast/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_ast-2">is_ast/2</a></td><td></td></tr><tr><td valign="top"><a href="#module_name-1">module_name/1</a></td><td>
Return the name of the module.</td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>
Parse the given AST and return.</td></tr><tr><td valign="top"><a href="#transform-3">transform/3</a></td><td>
Transform using the given fun.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_export-3"></a>

### add_export/3 ###

<pre><code>
add_export(PT_AST::<a href="#type-pt_ast">pt_ast()</a>, Name::atom(), Arity::integer()) -&gt; <a href="#type-pt_ast">pt_ast()</a>
</code></pre>
<br />

Declare a new function to export in the AST

Example:

```

  % export function/2
  pt_helpers:add_export(function, 2).
```

<a name="add_function-4"></a>

### add_function/4 ###

<pre><code>
add_function(PT_AST::<a href="#type-pt_ast">pt_ast()</a>, Visibility::export | not_export, Name::atom(), Clauses::tuple() | list()) -&gt; <a href="#type-pt_ast">pt_ast()</a>
</code></pre>
<br />

Add a function to the AST

Example:

```

  A = pt_helpers:build_var('A'),
  B = pt_helpers:build_var('B'),
  AIsNumber = pt_helpers:build_call(is_number, A),
  BIsNumber = pt_helpers:build_call(is_number, B),
  Guards = pt_helpers:build_and_guard(AIsNumber, BIsNumber),
  Body = pt_helpers:build_op('*', A, B),
  Clauses = pt_helpers:build_clause([A, B], Guards, Body),
  PT_AST1 = pt_helpers:add_function(PT_AST, export, my_function, Clauses)
  % => my_function(A, B) when is_number(A), is_number(B) -> A * B
```

<a name="add_record-3"></a>

### add_record/3 ###

<pre><code>
add_record(PT_AST::<a href="#type-pt_ast">pt_ast()</a>, Name::atom(), Attributes::list()) -&gt; <a href="#type-pt_ast">pt_ast()</a>
</code></pre>
<br />

Declare a new record to export in the AST

Example:

```

  pt_helpers:add_record(record_name, [{field1, integer}, {field2, []}, field3]).
```

<a name="build_and_guard-2"></a>

### build_and_guard/2 ###

<pre><code>
build_and_guard(Ast1::<a href="#type-ast">ast()</a>, Ast2::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

<a name="build_atom-1"></a>

### build_atom/1 ###

<pre><code>
build_atom(A::atom()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify an atom

<a name="build_bin-1"></a>

### build_bin/1 ###

`build_bin(S) -> any()`

ASTify a bin

<a name="build_boolean-1"></a>

### build_boolean/1 ###

`build_boolean(B) -> any()`

ASTify a boolean

<a name="build_call-2"></a>

### build_call/2 ###

<pre><code>
build_call(Function::atom(), Parameters::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a function call

Example:

```

  A = build_atom(atom),
  B = build_var('Var'),
  build_call(function, [A, B]) % == function(atom, Var)
```

<a name="build_call-3"></a>

### build_call/3 ###

<pre><code>
build_call(Module::atom() | <a href="#type-ast">ast()</a>, Function::atom() | <a href="#type-ast">ast()</a>, Parameters::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a function call

Example:

```

  A = build_atom(atom),
  B = build_var('Var'),
  build_call(module, function, [A, B]) % == module:function(atom, Var)
```

<a name="build_case-2"></a>

### build_case/2 ###

<pre><code>
build_case(Exp::<a href="#type-ast">ast()</a>, Clauses::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

<a name="build_clause-2"></a>

### build_clause/2 ###

<pre><code>
build_clause(Vars::<a href="#type-ast">ast()</a>, Body::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

<a name="build_clause-3"></a>

### build_clause/3 ###

<pre><code>
build_clause(Vars::<a href="#type-ast">ast()</a>, Guards::<a href="#type-ast">ast()</a>, Body::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

<a name="build_float-1"></a>

### build_float/1 ###

<pre><code>
build_float(F::float()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a float

<a name="build_fun-1"></a>

### build_fun/1 ###

`build_fun(Clauses) -> any()`

ASTify a fun

<a name="build_get_record_field-2"></a>

### build_get_record_field/2 ###

<pre><code>
build_get_record_field(RecordName::atom(), Field::atom()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a record_field

Example:

```

  build_get_record_field(record, field) % == record.field
```

<a name="build_get_record_field-3"></a>

### build_get_record_field/3 ###

<pre><code>
build_get_record_field(RecordVar::atom() | <a href="#type-ast">ast()</a>, RecordName::atom(), Field::atom()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a record_field

Example:

```

  build_get_record_field('R', record, field) % == R#record.field
```

<a name="build_guard-1"></a>

### build_guard/1 ###

<pre><code>
build_guard(Ast::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

<a name="build_if-1"></a>

### build_if/1 ###

<pre><code>
build_if(Clauses::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

<a name="build_integer-1"></a>

### build_integer/1 ###

<pre><code>
build_integer(I::integer()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify an integer

<a name="build_lc-2"></a>

### build_lc/2 ###

`build_lc(Var, Generates) -> any()`

<a name="build_lc_generate-2"></a>

### build_lc_generate/2 ###

`build_lc_generate(Var, List) -> any()`

<a name="build_list-1"></a>

### build_list/1 ###

<pre><code>
build_list(L::list()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a list

<a name="build_match-2"></a>

### build_match/2 ###

<pre><code>
build_match(A::<a href="#type-ast">ast()</a>, B::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a match (=)

<a name="build_op-3"></a>

### build_op/3 ###

<pre><code>
build_op(Op::atom(), A::<a href="#type-ast">ast()</a>, B::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify an operator

<a name="build_or_guard-2"></a>

### build_or_guard/2 ###

<pre><code>
build_or_guard(Ast1::<a href="#type-ast">ast()</a>, Ast2::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

<a name="build_record-1"></a>

### build_record/1 ###

`build_record(Name) -> any()`

ASTify a record

Example:

```

  pt_helpers:build_record(rec) % => #rec{}
```

<a name="build_record-2"></a>

### build_record/2 ###

`build_record(Name, Fields) -> any()`

ASTify a record

Example:

```

  F1 = pt_helpers:build_record_field(field1, pt_helpers:build_value(12)),
  F2 = pt_helpers:build_record_field(field2, pt_helpers:build_var('X'),
  pt_helpers:build_record(rec, [F1, F2]) % => #rec{field1 = 12, field2 = X}
```

<a name="build_record-3"></a>

### build_record/3 ###

`build_record(Record, Name, Fields) -> any()`

ASTify a record

Example:

```

  F1 = pt_helpers:build_record_field(field1, pt_helpers:build_value(12)),
  F2 = pt_helpers:build_record_field(field2, pt_helpers:build_var('X'),
  pt_helpers:build_record('R', rec, [F1, F2]) % => R#rec{field1 = 12, field2 = X}
```

<a name="build_record_field-2"></a>

### build_record_field/2 ###

<pre><code>
build_record_field(Field::atom(), Value::<a href="#type-ast">ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a record_field

Example:

```

  build_record_field(record, field) % == {record = field}
```

<a name="build_string-1"></a>

### build_string/1 ###

<pre><code>
build_string(S::string()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a string

<a name="build_tuple-1"></a>

### build_tuple/1 ###

<pre><code>
build_tuple(T::tuple()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a tuple

<a name="build_value-1"></a>

### build_value/1 ###

`build_value(X) -> any()`

ASTify the given value

<a name="build_var-1"></a>

### build_var/1 ###

<pre><code>
build_var(A::atom()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

ASTify a variable

<a name="directive-2"></a>

### directive/2 ###

<pre><code>
directive(Pt_ast::<a href="#type-ast">ast()</a>, Name::atom()) -&gt; list()
</code></pre>
<br />

Return the value for a directive Name

<a name="fields-1"></a>

### fields/1 ###

<pre><code>
fields(Pt_ast::<a href="#type-ast">ast()</a>) -&gt; list()
</code></pre>
<br />

Return the list of all availables fields

<a name="find_function-3"></a>

### find_function/3 ###

<pre><code>
find_function(Pt_ast::<a href="#type-pt_ast">pt_ast()</a>, Name::atom(), Arity::integer()) -&gt; {ok, <a href="#type-pt_fun">pt_fun()</a>} | {error, not_found}
</code></pre>
<br />

Find the function with name `Name` and arity `Arity` in the parsed AST

Example:

```

  parse_transform(AST, _Option) ->
    PT_AST = pt_helpers:parse(AST),
    PT_FUN = pt_helpers:find_function(PT_AST, my_function, 2).
    % Do something with PT_FUN
    pt_helpers(PT_AST).
```

<a name="find_functions-2"></a>

### find_functions/2 ###

<pre><code>
find_functions(Pt_ast::<a href="#type-pt_ast">pt_ast()</a>, Name::atom()) -&gt; {ok, <a href="#type-pt_fun">pt_fun()</a>} | {error, not_found}
</code></pre>
<br />

<a name="generate-1"></a>

### generate/1 ###

<pre><code>
generate(Pt_ast::<a href="#type-pt_ast">pt_ast()</a>) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

Generate the AST

Example:

```

  parse_transform(AST, Option) ->
    PT_AST = pt_helpers:parse(AST, Option),
    % Do something with PT_AST
    pt_helpers:generate(PT_AST).
```

<a name="get_ast_type-1"></a>

### get_ast_type/1 ###

<pre><code>
get_ast_type(E::<a href="#type-ast">ast()</a>) -&gt; {ok, atom()} | {error, wrong_ast}
</code></pre>
<br />

Return the type of the given AST

<a name="index-2"></a>

### index/2 ###

<pre><code>
index(Pt_ast::<a href="#type-pt_ast">pt_ast()</a>, I::integer()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

Return AST at index

<a name="is_ast-1"></a>

### is_ast/1 ###

<pre><code>
is_ast(AST::<a href="#type-ast">ast()</a> | [<a href="#type-ast">ast()</a>]) -&gt; true | false
</code></pre>
<br />

<a name="is_ast-2"></a>

### is_ast/2 ###

<pre><code>
is_ast(Type::atom(), AST::<a href="#type-ast">ast()</a> | [<a href="#type-ast">ast()</a>]) -&gt; true | false
</code></pre>
<br />

<a name="module_name-1"></a>

### module_name/1 ###

<pre><code>
module_name(Pt_ast::<a href="#type-pt_ast">pt_ast()</a>) -&gt; string()
</code></pre>
<br />

Return the name of the module

<a name="parse-2"></a>

### parse/2 ###

<pre><code>
parse(AST::string(), Options::list()) -&gt; <a href="#type-pt_ast">pt_ast()</a>
</code></pre>
<br />

Parse the given AST and return

Example:

```

  parse_transform(AST, Option) ->
    PT_AST = pt_helpers:parse(AST, Options),
    % Do something with PT_AST
    pt_helpers:generate(PT_AST).
```

<a name="transform-3"></a>

### transform/3 ###

<pre><code>
transform(Fun::function(), AST::string(), Options::list()) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

Transform using the given fun

Example:

```

  parse_transform(AST, Option) ->
    pt_helpers:transform(fun do_the_job/1, AST, Option).
  do_the_job(PT_AST) ->
    % Do something with PT_AST
    PT_ASTn.
```

