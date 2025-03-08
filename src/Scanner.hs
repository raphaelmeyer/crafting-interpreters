{-# LANGUAGE OverloadedStrings #-}

module Scanner (scanTokens) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Token

data Scanner = Scanner
  { sCurrent :: Int,
    sLine :: Int,
    sSource :: Text.Text
  }
  deriving (Eq, Show)

scanTokens :: Text.Text -> [Token.Token]
scanTokens source =
  fst $ scanUntil scanToken (Scanner 0 0 source)

scanUntil :: State.State Scanner (Maybe Token.Token) -> Scanner -> ([Token.Token], Scanner)
scanUntil m s = case State.runState m s of
  (Nothing, s') -> ([Token.Token Token.EOF "" (sLine s')], s')
  (Just token, s') -> add token (scanUntil m s')
    where
      add t (ts, final) = (t : ts, final)

scanToken :: State.State Scanner (Maybe Token.Token)
scanToken = do
  c <- advance
  s <- State.get
  case c of
    '(' -> pure . Just $ Token.Token Token.LeftParen "" (sLine s)
    ')' -> pure . Just $ Token.Token Token.RightParen "" (sLine s)
    _ -> pure Nothing

advance :: State.State Scanner Char.Char
advance = do
  s <- State.get
  let c = Text.head (sSource s)
  State.put
    ( Scanner
        { sCurrent = sCurrent s + 1,
          sLine = sLine s,
          sSource = Text.tail . sSource $ s
        }
    )
  pure c