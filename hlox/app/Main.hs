{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO as Text.IO
import qualified HLox
import qualified Runtime.Interpreter as Interpreter
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO

main :: IO ()
main = do
  args <- Environment.getArgs
  exitCode <- case args of
    ["-d", f] -> runFile HLox.PrintStmts f
    ["-d"] -> runPrompt HLox.PrintStmts
    [f] -> runFile HLox.Silent f
    [] -> runPrompt HLox.Silent
    _ -> usage
  Exit.exitWith exitCode

usage :: IO Exit.ExitCode
usage = do
  name <- Environment.getProgName
  System.IO.hPutStrLn System.IO.stderr $ "Usage: " ++ name ++ " (-d) [script]"
  pure $ Exit.ExitFailure 64

runFile :: HLox.Debug -> String -> IO Exit.ExitCode
runFile debug f = do
  script <- Text.IO.readFile f
  globals <- Interpreter.makeEnvironment
  HLox.run debug script globals

runPrompt :: HLox.Debug -> IO Exit.ExitCode
runPrompt debug = do
  globals <- Interpreter.makeEnvironment
  whilePrompt debug globals

whilePrompt :: HLox.Debug -> Interpreter.Environment -> IO Exit.ExitCode
whilePrompt debug globals = do
  Text.IO.putStr "> "
  System.IO.hFlush System.IO.stdout
  done <- System.IO.isEOF
  if done
    then
      pure Exit.ExitSuccess
    else do
      line <- Text.IO.getLine
      _ <- HLox.run debug line globals
      whilePrompt debug globals
