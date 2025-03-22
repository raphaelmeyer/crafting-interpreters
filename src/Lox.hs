module Lox where

import qualified Data.Text as Text

data Value
  = Number Double
  | String Text.Text
  | Boolean Bool
  | Nil
  deriving (Eq, Show)
