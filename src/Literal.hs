module Literal where

import qualified Data.Text as Text

data Value
  = Boolean Bool
  | Nil
  | Number Double
  | String Text.Text
  deriving (Eq, Show)
