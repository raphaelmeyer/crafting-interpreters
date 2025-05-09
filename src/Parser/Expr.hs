module Parser.Expr where

import qualified Data.Text as Text
import qualified Parser.Literal as Literal

data Expr = Expr Expression Location deriving (Eq, Show)

data Expression
  = Assign {assignName :: Identifier, assignExpr :: Expr}
  | Binary {binaryOp :: BinaryOp, binaryLeft :: Expr, binaryRight :: Expr}
  | Call {callCallee :: Expr, callArguments :: [Expr]}
  | Grouping Expr
  | Literal Literal.Value
  | Logical {logicalOp :: LogicalOp, logicalLeft :: Expr, logicalRight :: Expr}
  | Unary {unaryOp :: UnaryOp, unaryExpr :: Expr}
  | Variable Identifier
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

type Location = Int

type Identifier = Text.Text
