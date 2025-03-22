module Expr where

import qualified Lox

data Expr
  = Binary {binaryLeft :: Expr, binaryRight :: Expr, binaryOp :: BinaryOp}
  | Grouping Expr
  | Literal Lox.Value
  | Unary {unaryExpr :: Expr, unaryOp :: UnaryOp}
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
