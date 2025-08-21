module Parser.Stmt where

import qualified Parser.Expr as Expr

data Stmt
  = Block [Stmt]
  | Break
  | Class
      { cName :: Expr.Identifier,
        cSuperclass :: Maybe Expr.Superclass,
        cMethods :: [Function]
      }
  | Expression Expr.Expr
  | Fun Function
  | If
      { ifCondition :: Expr.Expr,
        thenStmt :: Stmt,
        elseStmt :: Maybe Stmt
      }
  | Print Expr.Expr
  | Return
      { retLoc :: Expr.Location,
        retValue :: Maybe Expr.Expr
      }
  | Variable
      { varName :: Expr.Identifier,
        varInitializer :: Expr.Expr
      }
  | While Expr.Expr Stmt
  deriving (Eq, Show)

data Function = Function
  { funName :: Expr.Identifier,
    funParameters :: [Expr.Identifier],
    funBody :: [Stmt]
  }
  deriving (Eq, Show)
