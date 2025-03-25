{-# LANGUAGE OverloadedStrings #-}

module ScannerSpec (spec) where

import Data.List.Extra ((!?))
import qualified Lox
import qualified Scanner
import Test.Hspec
import qualified Token

spec :: Spec
spec = do
  describe "empty string" $ do
    it "should return eof token" $ do
      let result = Scanner.scanTokens ""
      result `shouldSatisfy` ((== 1) . countTokens)
      result `shouldSatisfy` (hasType Token.EOF . nthToken 0)

  describe "single character tokens" $ do
    it "should scan a single token" $ do
      let result = Scanner.scanTokens "!"
      result `shouldSatisfy` ((== 2) . countTokens)
      result `shouldSatisfy` (hasType Token.Bang . nthToken 0)

countTokens :: Lox.Result [Token.Token] -> Int
countTokens result = case result of
  Left _ -> 0
  Right ts -> length ts

nthToken :: Int -> Lox.Result [Token.Token] -> Maybe Token.Token
nthToken n result = case result of
  Left _ -> Nothing
  Right ts -> ts !? n

hasType :: Token.TokenType -> Maybe Token.Token -> Bool
hasType tType token = case token of
  Nothing -> False
  Just t -> (== tType) . Token.tType $ t
