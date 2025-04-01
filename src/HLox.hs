module HLox (run) where

import qualified Data.Text as Text
import qualified Interpreter
import qualified Lox
import qualified Parser
import qualified Scanner

run :: Text.Text -> Lox.Result Lox.Value
run source = do
  tokens <- Scanner.scanTokens source
  stmts <- Parser.parse tokens
  Interpreter.interpret stmts
