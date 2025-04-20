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
    ["-d", f] -> runFile HLox.PrintStmts f
    ["-d"] -> runPrompt HLox.PrintStmts
    [f] -> runFile HLox.Silent f
    [] -> runPrompt HLox.Silent
    _ -> usage

usage :: IO ()
usage = do
  name <- Environment.getProgName
  System.IO.hPutStrLn System.IO.stderr $ "Usage: " ++ name ++ " (-d) [script]"
  Exit.exitFailure

runFile :: HLox.Debug -> String -> IO ()
runFile debug f = do
  script <- Text.IO.readFile f
  HLox.run debug script

runPrompt :: HLox.Debug -> IO ()
runPrompt debug = do
  Text.IO.putStr "> "
  System.IO.hFlush System.IO.stdout
  done <- System.IO.isEOF
  M.unless done $ do
    line <- Text.IO.getLine
    HLox.run debug line
    runPrompt debug
