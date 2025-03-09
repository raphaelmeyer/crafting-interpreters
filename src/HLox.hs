module HLox (run) where

import qualified Data.Text as Text
import qualified Error

type Result = Maybe Error.Error

success :: Result
success = Nothing

run :: Text.Text -> IO Result
run = const . pure $ success
