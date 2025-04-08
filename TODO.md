# TODO list

Some notes or ideas what maybe could be improved.

- split errors into runtime, parser and scanner errors
- add tests
- clean up return types for `scanTokens`, `parse` and `interpret`
- better understand monad stacks and how to prevent them, because when we add
  state to `interpret` we'll already of stack of three
- directory structure, split files and put into subdirectories
  - e.g. Interpreter/Statement.hs, Interpreter/Expression.hs...
- decide if Variable.Stmt should have (Maybe Expr) or just use literal Nil for no assignment
- add location (source) information to statements for error reporting
