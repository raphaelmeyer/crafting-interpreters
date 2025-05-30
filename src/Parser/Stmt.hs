module Parser.Stmt where

import qualified Parser.Expr as Expr

data Stmt
  = Block [Stmt]
  | Break
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
