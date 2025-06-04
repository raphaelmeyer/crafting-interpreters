module Resolver.Resolver (resolve) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lox
import qualified Parser.Stmt as Stmt

type Scope = Map.Map Text.Text Bool

type Resolver a = State.State [Scope] a

resolve :: [Stmt.Stmt] -> Lox.Result [Stmt.Stmt]
resolve stmts = Right $ State.evalState (program stmts) []

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
statement stmt = pure stmt

beginScope :: Resolver ()
beginScope = do
  State.modify (\scopes -> Map.empty : scopes)

endScope :: Resolver ()
endScope = do
  s <- State.get
  case s of
    [] -> pure ()
    (_ : scopes) -> State.put scopes
