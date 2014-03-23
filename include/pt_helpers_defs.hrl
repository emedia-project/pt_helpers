-record(pt_ast, {
  ast,
  options,
  main_file,
  compile_options,
  last_line,
  module_name,
  exports = [],
  exports_pos = -1,
  fields,
  functions = [],
  function_pos = -1,

  added_functions = [],
  added_exports = [],
  added_records = []
}).

-record(pt_fun, {
  index, 
  name, 
  visibility,
  arity,
  clauses,
  ast
}).

-record(pt_field, {
  index,
  data
}).

-record(pt_record, {
  index,
  name,
  fields,
  ast,
  ast_type,
  has_type
}).

-type pt_ast() :: #pt_ast{}.
-type pt_fun() :: #pt_fun{}.
-type ast() :: tuple() | [tuple()].

