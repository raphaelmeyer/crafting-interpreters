{-# LANGUAGE OverloadedStrings #-}

module Scanner (scanTokens) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Error
import qualified Token

data Scanner = Scanner
  { sCurrent :: Int,
    sLine :: Int,
    sSource :: Text.Text,
    sErrors :: [Error.Error]
  }
  deriving (Eq, Show)

newScanner :: Text.Text -> Scanner
newScanner source = Scanner 0 0 source []

scanTokens :: Text.Text -> Either [Error.Error] [Token.Token]
scanTokens source =
  let (tokens, s) = scanUntil scanToken . newScanner $ source
   in case sErrors s of
        [] -> Right tokens
        errors -> Left errors

scanUntil :: State.State Scanner (Maybe Token.Token) -> Scanner -> ([Token.Token], Scanner)
scanUntil m s = case State.runState m s of
  (Nothing, s') -> scanUntil m s'
  (Just t@(Token.Token Token.EOF _ _), s') -> ([t], s')
  (Just token, s') -> add token (scanUntil m s')
    where
      add t (ts, final) = (t : ts, final)

scanToken :: State.State Scanner (Maybe Token.Token)
scanToken = do
  maybeC <- advance
  s <- State.get
  case maybeC of
    Nothing -> pure . Just $ Token.Token Token.EOF "" (sLine s)
    Just c -> case c of
      '(' -> createToken Token.LeftParen c s
      ')' -> createToken Token.RightParen c s
      '{' -> createToken Token.LeftBrace c s
      '}' -> createToken Token.RightBrace c s
      ',' -> createToken Token.Comma c s
      '.' -> createToken Token.Dot c s
      '-' -> createToken Token.Minus c s
      '+' -> createToken Token.Plus c s
      ';' -> createToken Token.SemiColon c s
      '*' -> createToken Token.Star c s
      _ -> do
        State.modify (addError "Unexpected character.")
        pure Nothing
  where
    createToken t c = pure . Just . Token.Token t (Text.singleton c) . sLine
    addError e s = s {sErrors = Error.Error (sLine s) e : sErrors s}

advance :: State.State Scanner (Maybe Char.Char)
advance = do
  s <- State.get
  let source = sSource s
  if Text.null source
    then pure Nothing
    else do
      let c = Text.head (sSource s)
      State.put
        ( s
            { sCurrent = sCurrent s + 1,
              sSource = Text.tail . sSource $ s
            }
        )
      pure . Just $ c
