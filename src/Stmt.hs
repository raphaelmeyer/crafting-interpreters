module Stmt where

import qualified Expr

data Stmt
  = Block [Stmt]
  | Expression Expr.Expr
  | Function
      { funName :: Expr.Identifier,
        funParameters :: [Expr.Identifier],
        funBody :: [Stmt]
      }
  | If
      { ifCondition :: Expr.Expr,
        thenStmt :: Stmt,
        elseStmt :: Maybe Stmt
      }
  | Print Expr.Expr
  | Return Expr.Expr
  | Variable Expr.Identifier Expr.Expr
  | While Expr.Expr Stmt
  deriving (Eq, Show)
