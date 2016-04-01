-record(pt_ast, {
          options = [],
          file,
          module,
          compile = [],
          exports = [],
          records = [],
          types = [],
          attributes = #{},
          functions = [],
          unparsed = [],
          specs = #{},
          eof = 0
         }).

-record(pt_fun, {
          name,
          arity,
          clauses
         }).

-record(pt_record, {
          name,
          fields
         }).

-record(pt_type, {
          name,
          def,
          data
         }).

-record(pt_clause, {
          args,
          guards,
          body
         }).

-type pt_ast() :: #pt_ast{}.
-type pt_fun() :: #pt_fun{}.
-type pt_clause() :: #pt_clause{}.
-type ast() :: tuple() | [ast()].

