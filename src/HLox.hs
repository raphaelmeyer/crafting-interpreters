module HLox (run) where

import qualified Data.Text as Text
import qualified Error
import qualified Interpreter
import qualified Parser
import qualified Scanner

type Result = Maybe [Error.Error]

run :: Text.Text -> IO Result
run source = do
  case Scanner.scanTokens source of
    Left errors -> pure $ Just errors
    Right tokens -> do
      -- mapM_ print tokens
      case Parser.parse tokens of
        Left errors -> pure $ Just errors
        Right expr -> do
          -- print expr
          print . Interpreter.interpret $ expr
          pure Nothing
