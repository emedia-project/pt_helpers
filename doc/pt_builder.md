

# Module pt_builder #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#build_and_guard-2">build_and_guard/2</a></td><td></td></tr><tr><td valign="top"><a href="#build_atom-1">build_atom/1</a></td><td>
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
ASTify a variable.</td></tr></table>


<a name="functions"></a>

## Function Details ##

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

<pre><code>
build_lc(Var::<a href="#type-ast">ast()</a>, Generates::<a href="#type-ast">ast()</a> | [<a href="#type-ast">ast()</a>]) -&gt; <a href="#type-ast">ast()</a>
</code></pre>
<br />

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

  pt_builder:build_record(rec) % => #rec{}
```

<a name="build_record-2"></a>

### build_record/2 ###

`build_record(Name, Fields) -> any()`

ASTify a record

Example:

```

  F1 = pt_builder:build_record_field(field1, pt_builder:build_value(12)),
  F2 = pt_builder:build_record_field(field2, pt_builder:build_var('X'),
  pt_builder:build_record(rec, [F1, F2]) % => #rec{field1 = 12, field2 = X}
```

<a name="build_record-3"></a>

### build_record/3 ###

`build_record(Record, Name, Fields) -> any()`

ASTify a record

Example:

```

  F1 = pt_builder:build_record_field(field1, pt_builder:build_value(12)),
  F2 = pt_builder:build_record_field(field2, pt_builder:build_var('X'),
  pt_builder:build_record('R', rec, [F1, F2]) % => R#rec{field1 = 12, field2 = X}
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

