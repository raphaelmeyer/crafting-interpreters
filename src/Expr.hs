module Expr where

import qualified Data.Text as Text

data Expr
  = Binary {binaryLeft :: Expr, binaryRight :: Expr, binaryOp :: BinaryOp}
  | Grouping {groupExpr :: Expr}
  | Literal {literal :: LiteralValue}
  | Unary {unaryExpr :: Expr, unaryOp :: UnaryOp}
  deriving (Eq, Show)

data LiteralValue
  = Number Double
  | String Text.Text
  | Boolean Bool
  | Nil
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
