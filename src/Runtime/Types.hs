module Runtime.Types where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as Text
import qualified Error
import qualified Stmt

data Value
  = Boolean Bool
  | Callable Declaration
  | Nil
  | Number Double
  | String Text.Text
  deriving (Eq, Show)

data Declaration
  = Clock
  | Function
      { funName :: Text.Text,
        funParameters :: [Text.Text],
        funBody :: [Stmt.Stmt]
      }
  deriving (Eq, Show)

arity :: Declaration -> Int
arity Clock = 0
arity Function {funParameters = params} = length params

type Interpreter m v a = Except.ExceptT Error.Error (State.StateT v m) a
