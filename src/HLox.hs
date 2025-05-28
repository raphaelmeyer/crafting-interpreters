module HLox (run, Debug (..)) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.IO.Class as IOClass
import qualified Data.Text as Text
import qualified Lox
import qualified Parser.Parser as Parser
import qualified Runtime.Interpreter as Interpreter
import qualified Scanner.Scanner as Scanner
import qualified System.Exit as Exit
import qualified System.IO as System

data Debug = PrintStmts | Silent deriving (Eq, Show)

run :: Debug -> Text.Text -> Interpreter.Environment -> IO Exit.ExitCode
run debug source globals = do
  result <- Except.runExceptT $ do
    tokens <- Except.ExceptT . pure $ Scanner.scanTokens source
    stmts <- Except.ExceptT . pure $ Parser.parse tokens
    Monad.when (debug == PrintStmts) $ IOClass.liftIO $ mapM_ printStmt stmts
    Except.ExceptT $ Interpreter.interpret stmts globals
  case result of
    Left err -> exitCode err <$ mapM_ printError err
    Right _ -> pure Exit.ExitSuccess
  where
    printStmt stmt = putStrLn $ "[STMT] " ++ show stmt

printError :: (Show a) => a -> IO ()
printError = System.hPrint System.stderr

exitCode :: [Lox.Error] -> Exit.ExitCode
exitCode (Lox.RuntimeError {} : _) = Exit.ExitFailure 70
exitCode (_ : _) = Exit.ExitFailure 65
exitCode [] = Exit.ExitFailure 1
