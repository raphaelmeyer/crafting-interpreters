module Stmt where

import qualified Expr

data Stmt
  = Expression Expr.Expr
  | Print Expr.Expr
  deriving (Eq, Show)
