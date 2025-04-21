module Stmt where

import qualified Data.Text as Text
import qualified Expr

data Stmt
  = Block [Stmt]
  | Expression Expr.Expr
  | Function
      { funName :: Text.Text,
        funParameters :: [Text.Text],
        funBody :: [Stmt]
      }
  | If
      { ifCondition :: Expr.Expr,
        thenStmt :: Stmt,
        elseStmt :: Maybe Stmt
      }
  | Print Expr.Expr
  | Variable Text.Text Expr.Expr
  | While Expr.Expr Stmt
  deriving (Eq, Show)
