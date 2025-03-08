module HLox (run) where

import qualified Data.Text as Text

data Error = Error
  { eLine :: Int,
    eLocation :: Text.Text,
    eMessage :: Text.Text
  }
  deriving (Eq, Show)

type Result = Maybe Error

success :: Result
success = Nothing

run :: Text.Text -> IO Result
run = const . pure $ success
