{-# LANGUAGE OverloadedStrings #-}

module Scanner (scanTokens) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Error
import qualified Text.Read as Read
import qualified Token

data Scanner = Scanner
  { sCurrent :: Int,
    sLine :: Int,
    sSource :: Text.Text,
    sErrors :: [Error.Error]
  }
  deriving (Eq, Show)

initScanner :: Text.Text -> Scanner
initScanner source = Scanner 0 1 source []

scanTokens :: Text.Text -> Either [Error.Error] [Token.Token]
scanTokens source =
  case sErrors scanner of
    [] -> Right . Maybe.catMaybes $ tokens
    errors -> Left errors
  where
    (tokens, scanner) = scanToEnd (initScanner source)
    scanToEnd = runStateWhile notEnd scanToken
    notEnd (Just (Token.Token Token.EOF _ _)) = False
    notEnd _ = True

scanToken :: State.State Scanner (Maybe Token.Token)
scanToken = do
  maybeC <- advance
  case maybeC of
    Nothing -> eof
    Just c -> case c of
      '(' -> simpleToken Token.RightParen c
      ')' -> simpleToken Token.RightParen c
      '{' -> simpleToken Token.LeftBrace c
      '}' -> simpleToken Token.RightBrace c
      ',' -> simpleToken Token.Comma c
      '.' -> simpleToken Token.Dot c
      '-' -> simpleToken Token.Minus c
      '+' -> simpleToken Token.Plus c
      ';' -> simpleToken Token.SemiColon c
      '*' -> simpleToken Token.Star c
      '!' -> composed '=' Token.BangEqual Token.Bang c
      '=' -> composed '=' Token.EqualEqual Token.Equal c
      '<' -> composed '=' Token.LessEqual Token.Less c
      '>' -> composed '=' Token.GreaterEqual Token.Greater c
      '/' -> comment '/' Token.Slash c
      ' ' -> whitespace
      '\r' -> whitespace
      '\t' -> whitespace
      '\n' -> newline
      '"' -> stringLiteral
      _ -> do
        if Char.isDigit c
          then
            scanNumber c
          else
            if isAlpha c
              then
                scanIdentifier c
              else addError "Unexpected character."

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

makeToken :: Token.TokenType -> Text.Text -> Int -> Maybe Token.Token
makeToken token lexeme line = Just $ Token.Token token lexeme line

whitespace :: State.State Scanner (Maybe Token.Token)
whitespace = pure Nothing

newline :: State.State Scanner (Maybe Token.Token)
newline = do
  State.modify (\s -> s {sLine = sLine s + 1})
  pure Nothing

eof :: State.State Scanner (Maybe Token.Token)
eof = do
  s <- State.get
  pure $ makeToken Token.EOF "" (sLine s)

simpleToken :: Token.TokenType -> Char.Char -> State.State Scanner (Maybe Token.Token)
simpleToken t c = do
  s <- State.get
  pure $ makeToken t (Text.singleton c) (sLine s)

composed :: Char.Char -> Token.TokenType -> Token.TokenType -> Char.Char -> State.State Scanner (Maybe Token.Token)
composed expected match miss c = do
  s <- State.get
  let source = sSource s
  if Text.null source
    then pure . Just $ Token.Token miss (Text.singleton c) (sLine s)
    else
      if Text.head source == expected
        then do
          State.put (s {sCurrent = sCurrent s + 1, sSource = Text.tail source})
          pure . Just $ Token.Token match (Text.pack [c, expected]) (sLine s)
        else pure . Just $ Token.Token miss (Text.singleton c) (sLine s)

comment :: Char.Char -> Token.TokenType -> Char.Char -> State.State Scanner (Maybe Token.Token)
comment expected miss c = do
  s <- State.get
  let source = sSource s
  if Text.null source
    then pure . Just $ Token.Token miss (Text.singleton c) (sLine s)
    else
      if Text.head source == expected
        then do
          let (source', current') = consumeUntil '\n' (Text.tail source) (sCurrent s + 1)
          State.put (s {sCurrent = current', sSource = source'})
          pure Nothing
        else pure . Just $ Token.Token miss (Text.singleton c) (sLine s)

consumeUntil :: Char.Char -> Text.Text -> Int -> (Text.Text, Int)
consumeUntil c source current
  | Text.null source = (source, current)
  | Text.head source == c = (source, current)
  | otherwise = consumeUntil c (Text.tail source) (current + 1)

stringLiteral :: State.State Scanner (Maybe Token.Token)
stringLiteral = do
  s <- State.get
  let (s', maybeString) = scanString s
  State.put s'
  case maybeString of
    Just string -> pure . Just $ Token.Token (Token.String string) string (sCurrent s')
    Nothing -> addError "Unterminated string."

scanString :: Scanner -> (Scanner, Maybe Text.Text)
scanString s
  | Text.null (sSource s) = (s, Nothing)
  | Text.head (sSource s) == '"' = (s {sCurrent = sCurrent s + 1, sSource = Text.tail (sSource s)}, Just Text.empty)
  | Text.head (sSource s) == '\n' = appendChar (Text.head (sSource s)) $ scanString (s {sCurrent = sCurrent s + 1, sLine = sLine s + 1, sSource = Text.tail (sSource s)})
  | otherwise = appendChar (Text.head (sSource s)) $ scanString (s {sCurrent = sCurrent s + 1, sSource = Text.tail (sSource s)})
  where
    appendChar _ (s', Nothing) = (s', Nothing)
    appendChar c (s', Just str) = (s', Just (Text.cons c str))

scanNumber :: Char.Char -> State.State Scanner (Maybe Token.Token)
scanNumber first = do
  s <- State.get
  let (source', current', remaining) = scanInteger (sSource s) (sCurrent s)
  let number = Text.cons first remaining
  case Read.readMaybe . Text.unpack $ number of
    Nothing -> do
      State.put (s {sSource = source', sCurrent = current', sErrors = Error.Error (sLine s) "Invalid number." : sErrors s})
      pure Nothing
    Just value -> do
      State.put (s {sSource = source', sCurrent = current'})
      pure . Just $ Token.Token (Token.Number value) number (sLine s)

scanInteger :: Text.Text -> Int -> (Text.Text, Int, Text.Text)
scanInteger source current =
  case maybeNext of
    Nothing -> (source, current, Text.empty)
    Just (c, source') ->
      if c == '.'
        then
          if Text.null source'
            then (source, current, Text.empty)
            else
              if Char.isDigit (Text.head source')
                then
                  appendDigit (Text.head source) (scanFraction source' (current + 1))
                else (source, current, Text.empty)
        else
          if Char.isDigit c
            then appendDigit c (scanInteger source' (current + 1))
            else (source, current, Text.empty)
  where
    maybeNext = Text.uncons source
    appendDigit c (source', current', num) = (source', current', Text.cons c num)

scanFraction :: Text.Text -> Int -> (Text.Text, Int, Text.Text)
scanFraction source current =
  case maybeNext of
    Nothing -> (source, current, Text.empty)
    Just (c, source') ->
      if Char.isDigit c
        then appendDigit c (scanFraction source' (current + 1))
        else (source, current, Text.empty)
  where
    maybeNext = Text.uncons source
    appendDigit c (source', current', num) = (source', current', Text.cons c num)

scanIdentifier :: Char.Char -> State.State Scanner (Maybe Token.Token)
scanIdentifier first = do
  s <- State.get
  let (source', current', remaining) = scanAlphaNumeric (sSource s) (sCurrent s)
  let identifier = Text.cons first remaining
  State.put (s {sSource = source', sCurrent = current'})
  pure . Just $ Token.Token (keywordOrIdentifier identifier) identifier (sLine s)

scanAlphaNumeric :: Text.Text -> Int -> (Text.Text, Int, Text.Text)
scanAlphaNumeric source current
  | Text.null source = (source, current, Text.empty)
  | isAlphaNumeric (Text.head source) = appendChar (Text.head source) (scanAlphaNumeric (Text.tail source) (current + 1))
  | otherwise = (source, current, Text.empty)
  where
    appendChar c (source', current', identifier) = (source', current', Text.cons c identifier)

isAlpha :: Char.Char -> Bool
isAlpha c = c == '_' || (Char.isLetter c && Char.isAscii c)

isAlphaNumeric :: Char.Char -> Bool
isAlphaNumeric c = isAlpha c || Char.isDigit c

keywordOrIdentifier :: Text.Text -> Token.TokenType
keywordOrIdentifier identifier =
  case identifier of
    "and" -> Token.And
    "class" -> Token.Class
    "else" -> Token.Else
    "false" -> Token.False
    "for" -> Token.For
    "fun" -> Token.For
    "if" -> Token.If
    "nil" -> Token.Nil
    "or" -> Token.Or
    "print" -> Token.Print
    "return" -> Token.Return
    "super" -> Token.Super
    "this" -> Token.This
    "true" -> Token.True
    "var" -> Token.Var
    "while" -> Token.While
    _ -> Token.Identifier identifier

addError :: Text.Text -> State.State Scanner (Maybe Token.Token)
addError e = do
  State.modify (\s -> s {sErrors = Error.Error (sLine s) e : sErrors s})
  pure Nothing

runStateWhile :: (Maybe a -> Bool) -> State.State s (Maybe a) -> s -> ([Maybe a], s)
runStateWhile continue m s =
  if continue result then addFst result (runStateWhile continue m s') else ([result], s')
  where
    (result, s') = State.runState m s

addFst :: a -> ([a], b) -> ([a], b)
addFst x (xs, y) = (x : xs, y)
