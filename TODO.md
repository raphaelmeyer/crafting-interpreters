# TODO list

Some notes or ideas what maybe could be improved.

- add tests
- alternative for `Parser.match` to just match without returning the token, e.g.
  for `isSemicolon`
- clean up return types for `scanTokens`, `parse` and `interpret`
- better understand monad stacks and how to prevent them, because when we add
  state to `interpret` we'll already of stack of three
- directory structure, split files and put into subdirectories
  - e.g. Interpreter/Statement.hs, Interpreter/Expression.hs...
