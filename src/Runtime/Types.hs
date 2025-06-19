{-# LANGUAGE InstanceSigs #-}

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
  deriving (Eq, Show)

data Declaration
  = Clock
  | Function
      { funName :: Text.Text,
        funParameters :: [Text.Text],
        funBody :: [Stmt.Stmt],
        funClosure :: Environment
      }
  | Class ClassDecl
  deriving (Eq, Show)

data ClassDecl = ClassDecl
  { clName :: Text.Text
  }
  deriving (Eq, Show)

data ClassInstance = ClassInstance
  { instClass :: ClassDecl
  }
  deriving (Eq, Show)

type Storage = Map.Map Text.Text Value

type Scope = IORef.IORef Storage

data Environment = Global Scope | Local Scope Environment deriving (Eq)

arity :: Declaration -> Int
arity Clock = 0
arity Class {} = 0
arity Function {funParameters = params} = length params

type Interpreter m a = Except.ExceptT Lox.Error (State.StateT Environment m) a

instance Show Environment where
  show :: Environment -> String
  show (Global _) = "Global"
  show (Local _ parent) = "Local { " ++ show parent ++ " }"
