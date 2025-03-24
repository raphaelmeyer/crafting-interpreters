module Lox where

import qualified Data.Text as Text
import qualified Error

type Result a = Either [Error.Error] a

data Value
  = Number Double
  | String Text.Text
  | Boolean Bool
  | Nil
  deriving (Eq, Show)
