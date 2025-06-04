module Resolver.Resolver (resolve) where

import qualified Lox
import qualified Parser.Stmt as Stmt

resolve :: [Stmt.Stmt] -> Lox.Result [Stmt.Stmt]
resolve stmts = Right $ program stmts

program :: [Stmt.Stmt] -> [Stmt.Stmt]
program = map statement

statement :: Stmt.Stmt -> Stmt.Stmt
statement = id
