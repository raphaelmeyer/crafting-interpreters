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
      '(' -> simpleToken c Token.RightParen
      ')' -> simpleToken c Token.RightParen
      '{' -> simpleToken c Token.LeftBrace
      '}' -> simpleToken c Token.RightBrace
      ',' -> simpleToken c Token.Comma
      '.' -> simpleToken c Token.Dot
      '-' -> simpleToken c Token.Minus
      '+' -> simpleToken c Token.Plus
      ';' -> simpleToken c Token.SemiColon
      '*' -> simpleToken c Token.Star
      '!' -> composed c '=' Token.BangEqual Token.Bang
      '=' -> composed c '=' Token.EqualEqual Token.Equal
      '<' -> composed c '=' Token.LessEqual Token.Less
      '>' -> composed c '=' Token.GreaterEqual Token.Greater
      '/' -> comment c
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

whitespace :: State.State Scanner (Maybe Token.Token)
whitespace = pure Nothing

newline :: State.State Scanner (Maybe Token.Token)
newline = do
  State.modify (\s -> s {sLine = sLine s + 1})
  pure Nothing

eof :: State.State Scanner (Maybe Token.Token)
eof = makeToken Token.EOF "" . sLine <$> State.get

simpleToken :: Char.Char -> Token.TokenType -> State.State Scanner (Maybe Token.Token)
simpleToken c t = makeToken t (Text.singleton c) . sLine <$> State.get

composed :: Char.Char -> Char.Char -> Token.TokenType -> Token.TokenType -> State.State Scanner (Maybe Token.Token)
composed first expected matchToken missToken = do
  m <- match expected
  case m of
    Nothing -> simpleToken first missToken
    Just second -> makeToken matchToken (Text.pack [first, second]) . sLine <$> State.get

comment :: Char.Char -> State.State Scanner (Maybe Token.Token)
comment first = do
  m <- match '/'
  case m of
    Nothing -> simpleToken first Token.Slash
    Just _ -> do
      State.modify $ dropUntil '\n'
      pure Nothing
  where
    dropUntil x s
      | Text.null (sSource s) = s
      | Text.head (sSource s) == x = s
      | otherwise = dropUntil x s {sCurrent = sCurrent s + 1, sSource = Text.tail . sSource $ s}

stringLiteral :: State.State Scanner (Maybe Token.Token)
stringLiteral = do
  maybeString <- State.state scanString
  case maybeString of
    Just string -> makeToken (Token.String string) string . sLine <$> State.get
    Nothing -> addError "Unterminated string."

scanString :: Scanner -> (Maybe Text.Text, Scanner)
scanString s
  | Text.null source = (Nothing, s)
  | Text.head source == '"' = (Just Text.empty, skip s)
  | Text.head source == '\n' = addChar . scanString . nextLine . skip $ s
  | otherwise = addChar . scanString . skip $ s
  where
    source = sSource s
    addChar (Nothing, s') = (Nothing, s')
    addChar (Just str, s') = (Just $ Text.cons (Text.head source) str, s')

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

advance :: State.State Scanner (Maybe Char.Char)
advance = do
  s <- State.get
  let source = sSource s
  if Text.null source
    then pure Nothing
    else do
      State.modify skip
      pure . Just $ Text.head source

match :: Char.Char -> State.State Scanner (Maybe Char.Char)
match m = do
  s <- State.get
  let source = sSource s
  if Text.null source
    then pure Nothing
    else do
      let c = Text.head source
      if c == m
        then do
          State.modify skip
          pure . Just $ c
        else
          pure Nothing

makeToken :: Token.TokenType -> Text.Text -> Int -> Maybe Token.Token
makeToken token lexeme line = Just $ Token.Token token lexeme line

addError :: Text.Text -> State.State Scanner (Maybe Token.Token)
addError e = do
  State.modify (\s -> s {sErrors = Error.Error (sLine s) e : sErrors s})
  pure Nothing

skip :: Scanner -> Scanner
skip s = s {sCurrent = sCurrent s + 1, sSource = Text.tail . sSource $ s}

nextLine :: Scanner -> Scanner
nextLine s = s {sLine = sLine s + 1}

runStateWhile :: (Maybe a -> Bool) -> State.State s (Maybe a) -> s -> ([Maybe a], s)
runStateWhile continue m s =
  if continue result then addFst result (runStateWhile continue m s') else ([result], s')
  where
    (result, s') = State.runState m s

addFst :: a -> ([a], b) -> ([a], b)
addFst x (xs, y) = (x : xs, y)
