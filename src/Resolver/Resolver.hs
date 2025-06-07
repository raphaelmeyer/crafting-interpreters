module Resolver.Resolver (resolve) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Lox
import qualified Parser.Expr as Expr
import qualified Parser.Stmt as Stmt

type Scope = Map.Map Expr.Identifier Bool

data ResolverState = ResolverState
  { rScopes :: [Scope],
    rErrors :: [Lox.Error]
  }

type Resolver a = State.State ResolverState a

resolve :: [Stmt.Stmt] -> Lox.Result [Stmt.Stmt]
resolve stmts = case rErrors s of
  [] -> Right resolved
  errors -> Left errors
  where
    (resolved, s) = State.runState (program stmts) (ResolverState [] [])

program :: [Stmt.Stmt] -> Resolver [Stmt.Stmt]
program [] = pure []
program (stmt : stmts) = do
  resolved <- statement stmt
  rs <- program stmts
  pure (resolved : rs)

statement :: Stmt.Stmt -> Resolver Stmt.Stmt
statement (Stmt.Block block) = do
  beginScope
  resolved <- program block
  endScope
  pure $ Stmt.Block resolved
statement (Stmt.Variable name initializer) = do
  declare name
  resolved <- expression initializer
  define name
  pure $ Stmt.Variable name resolved
statement stmt = pure stmt

expression :: Expr.Expr -> Resolver Expr.Expr
expression expr = pure expr

declare :: Expr.Identifier -> Resolver ()
declare name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : scopes) -> State.put s {rScopes = Map.insert name False scope : scopes}

define :: Expr.Identifier -> Resolver ()
define name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : scopes) -> State.put s {rScopes = Map.insert name True scope : scopes}

beginScope :: Resolver ()
beginScope = do
  State.modify $ \s -> s {rScopes = Map.empty : rScopes s}

endScope :: Resolver ()
endScope = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (_ : scopes) -> State.put s {rScopes = scopes}
