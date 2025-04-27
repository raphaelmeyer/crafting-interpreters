module Runtime.Types where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
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

type Scope = Map.Map Text.Text Value

data Values = Global Scope | Local Scope Values

type Environment s = s Values

arity :: Declaration -> Int
arity Clock = 0
arity Function {funParameters = params} = length params
