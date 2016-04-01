

# Module pt_ast #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_export-3">add_export/3</a></td><td></td></tr><tr><td valign="top"><a href="#add_function-4">add_function/4</a></td><td>
Add a function to the AST.</td></tr><tr><td valign="top"><a href="#find_function-3">find_function/3</a></td><td>
Seeach for the function <tt>Name</tt> with <tt>Arity</tt> and return the correspondive <tt>#pt_fun{}</tt></td></tr><tr><td valign="top"><a href="#generate-1">generate/1</a></td><td>
Generate the AST.</td></tr><tr><td valign="top"><a href="#parse-2">parse/2</a></td><td>
Parse the given AST and return.</td></tr><tr><td valign="top"><a href="#remove_export-3">remove_export/3</a></td><td></td></tr><tr><td valign="top"><a href="#remove_function-3">remove_function/3</a></td><td></td></tr><tr><td valign="top"><a href="#transform-3">transform/3</a></td><td>
Transform using the given fun.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_export-3"></a>

### add_export/3 ###

<pre><code>
add_export(Pt_ast::<a href="#type-pt_ast">pt_ast()</a>, Name::atom(), Arity::integer()) -&gt; <a href="#type-pt_ast">pt_ast()</a>
</code></pre>
<br />

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

<a name="find_function-3"></a>

### find_function/3 ###

<pre><code>
find_function(PT_AST::<a href="#type-pt_ast">pt_ast()</a>, Name::atom(), Arity::integer()) -&gt; {ok, <a href="#type-pt_fun">pt_fun()</a>} | not_found
</code></pre>
<br />

Seeach for the function `Name` with `Arity` and return the correspondive `#pt_fun{}`

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
    PT_AST = pt_ast:parse(AST, Option),
    % Do something with PT_AST
    pt_ast:generate(PT_AST).
```

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
    PT_AST = pt_ast:parse(AST, Options),
    % Do something with PT_AST
    pt_ast:generate(PT_AST).
```

<a name="remove_export-3"></a>

### remove_export/3 ###

`remove_export(Pt_ast, Name, Arity) -> any()`

<a name="remove_function-3"></a>

### remove_function/3 ###

`remove_function(Pt_ast, Name, Arity) -> any()`

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
    pt_ast:transform(fun do_the_job/1, AST, Option).
  do_the_job(PT_AST) ->
    % Do something with PT_AST
    PT_ASTn.
```

