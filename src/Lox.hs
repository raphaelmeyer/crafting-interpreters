module Lox where

import qualified Data.Text as Text
import qualified Error

type Result a = Either [Error.Error] a

data Value
  = Boolean Bool
  | Callable Declaration
  | Nil
  | Number Double
  | String Text.Text
  deriving (Eq, Show)

data Declaration = Clock deriving (Eq, Show)

arity :: Declaration -> Int
arity Clock = 0
