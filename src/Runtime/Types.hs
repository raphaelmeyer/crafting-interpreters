module Runtime.Types where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lox
import qualified Parser.Stmt as Stmt

data Value
  = Boolean Bool
  | Callable Declaration
  | Instance ClassInstance
  | Nil
  | Number Double
  | String Text.Text
  deriving (Eq)

data Declaration
  = Clock
  | Function FunctionDecl
  | Class ClassDecl
  deriving (Eq)

data ClassDecl = ClassDecl
  { clName :: Text.Text,
    clSuper :: Maybe ClassDecl,
    clMethods :: IORef.IORef (Map.Map Text.Text FunctionDecl),
    clArity :: Maybe Int
  }
  deriving (Eq)

data FunctionDecl = FunctionDecl
  { funName :: Text.Text,
    funParameters :: [Text.Text],
    funBody :: [Stmt.Stmt],
    funClosure :: Environment,
    funIsInitializer :: Bool
  }
  deriving (Eq)

data ClassInstance = ClassInstance
  { instClass :: ClassDecl,
    instFields :: IORef.IORef (Map.Map Text.Text Value)
  }
  deriving (Eq)

type Storage = Map.Map Text.Text Value

type Scope = IORef.IORef Storage

data Environment = Global Scope | Local Scope Environment deriving (Eq)

type Interpreter m a = Except.ExceptT Lox.Error (State.StateT Environment m) a
