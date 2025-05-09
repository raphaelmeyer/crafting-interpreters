{-# LANGUAGE OverloadedStrings #-}

module Scanner.Scanner (scanTokens) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Lox
import qualified Scanner.Token as Token
import qualified Text.Read as Read

data Scanner = Scanner
  { sCurrent :: Int,
    sLine :: Int,
    sSource :: Text.Text,
    sErrors :: [Lox.Error]
  }
  deriving (Eq, Show)

scanTokens :: Text.Text -> Lox.Result [Token.Token]
scanTokens source =
  case sErrors scanner of
    [] -> Right . Maybe.catMaybes $ tokens
    errors -> Left errors
  where
    (tokens, scanner) = scanToEnd (initScanner source)
    scanToEnd = runStateWhile (not . atEnd) scanToken

initScanner :: Text.Text -> Scanner
initScanner source = Scanner 0 1 source []

scanToken :: State.State Scanner (Maybe Token.Token)
scanToken = do
  maybeC <- advance
  case maybeC of
    Nothing -> eof
    Just c -> case c of
      '(' -> simpleToken c Token.LeftParen
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
      _ -> case letter c of
        Digit -> number c
        Alpha -> identifier c
        _ -> addError "Unexpected character."

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

number :: Char.Char -> State.State Scanner (Maybe Token.Token)
number first = do
  remaining <- scanNumber
  let num = Text.cons first remaining
  case Read.readMaybe . Text.unpack $ num of
    Just value -> do
      makeToken (Token.Number value) num . sLine <$> State.get
    Nothing -> addError (Text.append "Invalid number: " num)

scanNumber :: State.State Scanner Text.Text
scanNumber = do
  integer <- State.state scanInteger
  hasFraction <- State.gets checkFraction
  if hasFraction
    then do
      State.modify skip
      fraction <- State.state scanInteger
      pure $ Text.append integer . Text.cons '.' $ fraction
    else pure integer

scanInteger :: Scanner -> (Text.Text, Scanner)
scanInteger s
  | Text.null source = (Text.empty, s)
  | Char.isDigit . Text.head $ source = addChar . scanInteger . skip $ s
  | otherwise = (Text.empty, s)
  where
    source = sSource s
    addChar (integer, s') = (Text.cons (Text.head source) integer, s')

checkFraction :: Scanner -> Bool
checkFraction s =
  case Text.uncons . sSource $ s of
    Just ('.', source') -> case Text.uncons source' of
      Just (d, _) -> Char.isDigit d
      _ -> False
    _ -> False

identifier :: Char.Char -> State.State Scanner (Maybe Token.Token)
identifier first = do
  remaining <- State.state scanAlphaNumeric
  let ident = Text.cons first remaining
  makeToken (checkKeyword ident) ident . sLine <$> State.get

scanAlphaNumeric :: Scanner -> (Text.Text, Scanner)
scanAlphaNumeric s
  | Text.null source = (Text.empty, s)
  | isAlphaNumeric (Text.head source) = addChar . scanAlphaNumeric . skip $ s
  | otherwise = (Text.empty, s)
  where
    source = sSource s
    addChar (ident, s') = (Text.cons (Text.head source) ident, s')

checkKeyword :: Text.Text -> Token.TokenType
checkKeyword ident =
  case ident of
    "and" -> Token.And
    "class" -> Token.Class
    "else" -> Token.Else
    "false" -> Token.False
    "for" -> Token.For
    "fun" -> Token.Fun
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
    _ -> Token.Identifier ident

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
  State.modify (\s -> s {sErrors = Lox.ScanError (sLine s) e : sErrors s})
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

atEnd :: Maybe Token.Token -> Bool
atEnd (Just (Token.Token Token.EOF _ _)) = True
atEnd _ = False

addFst :: a -> ([a], b) -> ([a], b)
addFst x (xs, y) = (x : xs, y)

data AlphaNumeric = Alpha | Digit | Other

letter :: Char.Char -> AlphaNumeric
letter c
  | isAlpha c = Alpha
  | Char.isDigit c = Digit
  | otherwise = Other

isAlpha :: Char.Char -> Bool
isAlpha c = c == '_' || (Char.isLetter c && Char.isAscii c)

isAlphaNumeric :: Char.Char -> Bool
isAlphaNumeric c = isAlpha c || Char.isDigit c
