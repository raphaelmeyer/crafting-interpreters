module Expr where

import qualified Data.Text as Text
import qualified Lox

data Expr
  = Assign {assignName :: Text.Text, assignExpr :: Expr}
  | Binary {binaryOp :: BinaryOp, binaryLeft :: Expr, binaryRight :: Expr}
  | Grouping Expr
  | Literal Lox.Value
  | Logical {logicalOp :: LogicalOp, logicalLeft :: Expr, logicalRight :: Expr}
  | Unary {unaryOp :: UnaryOp, unaryExpr :: Expr}
  | Variable Text.Text
  deriving (Eq, Show)

data UnaryOp = Neg | Not deriving (Eq, Show)

data BinaryOp
  = Plus
  | Minus
  | Mult
  | Div
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  deriving (Eq, Show)

data LogicalOp = And | Or deriving (Eq, Show)
