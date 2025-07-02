module Parser.Expr where

import qualified Data.Text as Text
import qualified Parser.Literal as Literal

data Expr = Expr Expression Location deriving (Eq, Show)

data Expression
  = Assign {assignName :: Identifier, assignExpr :: Expr, assignDepth :: Maybe Int}
  | Binary {binaryOp :: BinaryOp, binaryLeft :: Expr, binaryRight :: Expr}
  | Call {callCallee :: Expr, callArguments :: [Expr]}
  | Get {getObject :: Expr, getName :: Identifier}
  | Grouping Expr
  | Literal Literal.Value
  | Logical {logicalOp :: LogicalOp, logicalLeft :: Expr, logicalRight :: Expr}
  | Set {setObject :: Expr, setName :: Identifier, setValue :: Expr}
  | Super {superMethod :: Identifier}
  | This {thisDepth :: Maybe Int}
  | Unary {unaryOp :: UnaryOp, unaryExpr :: Expr}
  | Variable {varName :: Identifier, varDepth :: Maybe Int}
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

data Identifier = Identifier
  { idName :: Text.Text,
    idLocation :: Location
  }
  deriving (Eq, Show)

data Superclass = Superclass
  { supName :: Identifier,
    supDepth :: Maybe Int
  }
  deriving (Eq, Show)
