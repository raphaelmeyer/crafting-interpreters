# TODO list

Some notes or ideas what maybe could be improved.

- check if environment gets simpler if Values is replaced with list as in []
- add tests
- clean up return types for `scanTokens`, `parse` and `interpret`
- directory structure, split files and put into subdirectories
  - e.g. Interpreter/Statement.hs, Interpreter/Expression.hs...
- decide if Variable.Stmt should have (Maybe Expr) or just use literal Nil for no assignment
- add location (source) information to statements for error reporting
