module HLox (run) where

import qualified Control.Monad.Except as Except
import qualified Data.Text as Text
import qualified Interpreter
import qualified Parser
import qualified Scanner

run :: Text.Text -> IO ()
run source = do
  result <- Except.runExceptT $ do
    tokens <- Except.ExceptT . pure $ Scanner.scanTokens source
    stmts <- Except.ExceptT . pure $ Parser.parse tokens
    Except.ExceptT $ Interpreter.interpret stmts
  case result of
    Left err -> mapM_ print err
    Right _ -> pure ()
