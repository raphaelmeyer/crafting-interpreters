module Expr where

import qualified Data.Text as Text
import qualified Lox

data Expr
  = Assign {assignName :: Text.Text, assignExpr :: Expr}
  | Binary {binaryLeft :: Expr, binaryRight :: Expr, binaryOp :: BinaryOp}
  | Grouping Expr
  | Literal Lox.Value
  | Logical {logicalLeft :: Expr, logicalRight :: Expr, logicalOp :: LogicalOp}
  | Unary {unaryExpr :: Expr, unaryOp :: UnaryOp}
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
