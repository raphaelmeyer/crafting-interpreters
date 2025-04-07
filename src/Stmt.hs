module Stmt where

import qualified Data.Text as Text
import qualified Expr

data Stmt
  = Expression Expr.Expr
  | Print Expr.Expr
  | Variable Text.Text Expr.Expr
  deriving (Eq, Show)
