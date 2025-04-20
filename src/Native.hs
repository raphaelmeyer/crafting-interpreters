module Native (clock) where

import qualified Data.Time.Clock.System as Clock
import qualified Lox

clock :: IO Lox.Value
clock = do
  now <- Clock.getSystemTime
  let seconds = (fromIntegral . Clock.systemSeconds) now
      milliseconds = fromIntegral . (`div` 1000000) . Clock.systemNanoseconds $ now
  pure $ Lox.Number (seconds + milliseconds / 1000.0)
