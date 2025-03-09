{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad as M
import Data.Text.IO as Text.IO
import qualified HLox
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO

main :: IO ()
main = do
  args <- Environment.getArgs
  case args of
    [f] -> runFile f
    [] -> runPrompt
    _ -> usage

usage :: IO ()
usage = do
  name <- Environment.getProgName
  System.IO.hPutStrLn System.IO.stderr $ "Usage: " ++ name ++ " [script]"
  Exit.exitFailure

runFile :: String -> IO ()
runFile f = do
  script <- Text.IO.readFile f
  result <- HLox.run script
  case result of
    Nothing -> pure ()
    Just errors -> do
      mapM_ print errors
      Exit.exitFailure

runPrompt :: IO ()
runPrompt = do
  Text.IO.putStr "> "
  System.IO.hFlush System.IO.stdout
  done <- System.IO.isEOF
  M.unless done $ do
    line <- Text.IO.getLine
    result <- HLox.run line
    case result of
      Nothing -> pure ()
      Just errors -> mapM_ print errors
    runPrompt
