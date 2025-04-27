# TODO list

Some notes or ideas what maybe could be improved.

- type for native functions, including name, ..., `fn :: ([Value] -> Value)` or similar
- move Expr.VariableName to new identifier module and use in environment etc
- move location to extra module ? ^^^ ???
- add missing location information
- test against test suite from book
- clean up return types for `scanTokens`, `parse` and `interpret`
- directory structure, split files and put into subdirectories
  - e.g. Interpreter/Statement.hs, Interpreter/Expression.hs...
- add tests
  - or convert and integrate test suite from book
