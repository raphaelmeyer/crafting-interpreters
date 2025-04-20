module HLox (run, Debug (..)) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IOClass
import qualified Data.Text as Text
import qualified Interpreter
import qualified Parser
import qualified Scanner

data Debug = PrintStmts | Silent deriving (Eq, Show)

run :: Debug -> Text.Text -> IO ()
run debug source = do
  result <- Except.runExceptT $ do
    tokens <- Except.ExceptT . pure $ Scanner.scanTokens source
    stmts <- Except.ExceptT . pure $ Parser.parse tokens
    Monad.when (debug == PrintStmts) $ IOClass.liftIO $ mapM_ printStmt stmts
    Except.ExceptT $ Interpreter.interpret stmts
  case result of
    Left err -> mapM_ print err
    Right _ -> pure ()
  where
    printStmt stmt = putStrLn $ "[STMT] " ++ show stmt
