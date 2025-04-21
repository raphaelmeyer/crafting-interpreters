module Runtime where

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
      { funParameters :: [Text.Text],
        funBody :: [Stmt.Stmt]
      }
  deriving (Eq, Show)

arity :: Declaration -> Int
arity Clock = 0
arity Function {funParameters = params} = length params
