{-# LANGUAGE OverloadedStrings #-}

module Scanner (scanTokens) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Text as Text
import qualified Token

data Scanner = Scanner
  { sCurrent :: Int,
    sLine :: Int
  }
  deriving (Eq, Show)

scanTokens :: Text.Text -> [Token.Token]
scanTokens _ =
  fst $ scanUntil scanToken (Scanner 0 0)

scanUntil :: State.State Scanner (Maybe Token.Token) -> Scanner -> ([Token.Token], Scanner)
scanUntil m s = case State.runState m s of
  (Nothing, s') -> ([Token.Token Token.EOF "" 0], s')
  (Just token, s') -> add token (scanUntil m s')
    where
      add t (ts, final) = (t : ts, final)

scanToken :: State.State Scanner (Maybe Token.Token)
scanToken = do
  s <- State.get
  State.put s
  pure Nothing
