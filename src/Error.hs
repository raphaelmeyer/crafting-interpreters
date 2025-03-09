module Error where

import qualified Data.Text as Text

data Error = Error
  { eLine :: Int,
    eMessage :: Text.Text
  }
  deriving (Eq, Show)