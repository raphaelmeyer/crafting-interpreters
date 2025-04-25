module Expr where

import qualified Data.Text as Text
import qualified Literal

data Expr
  = Assign {assignName :: VariableName, assignExpr :: Expr}
  | Binary {binaryOp :: BinaryOp, binaryLeft :: Expr, binaryRight :: Expr}
  | Call {callCallee :: Expr, callArguments :: [Expr]}
  | Grouping Expr
  | Literal Literal.Value
  | Logical {logicalOp :: LogicalOp, logicalLeft :: Expr, logicalRight :: Expr}
  | Unary {unaryOp :: UnaryOp, unaryExpr :: Expr}
  | Variable VariableName
  deriving (Eq, Show)

data UnaryOperation = Neg | Not deriving (Eq, Show)

data BinaryOperation
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

data LogicalOperation = And | Or deriving (Eq, Show)

type Location = Int

data Operator a = Operator a Location deriving (Eq, Show)

type UnaryOp = Operator UnaryOperation

type BinaryOp = Operator BinaryOperation

type LogicalOp = Operator LogicalOperation

data VariableName = VariableName Text.Text Location deriving (Eq, Show)
