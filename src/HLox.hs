module HLox (run) where

import qualified Data.Text as Text
import qualified Error
import qualified Parser
import qualified Scanner

type Result = Maybe [Error.Error]

run :: Text.Text -> IO Result
run source = do
  case Scanner.scanTokens source of
    Left errors -> pure $ Just errors
    Right tokens -> do
      mapM_ print tokens
      let expr = Parser.parse tokens
      print expr
      pure Nothing
