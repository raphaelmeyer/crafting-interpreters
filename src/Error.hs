module Error where

import qualified Data.Text as Text

data Error = Error
  { eLine :: Int,
    eMessage :: Text.Text
  }
  deriving (Eq)

instance Show Error where
  show e = "[Line " ++ line ++ "] Error: " ++ message
    where
      line = show . eLine $ e
      message = Text.unpack . eMessage $ e
