{-# LANGUAGE OverloadedStrings #-}

module Parser (parse) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Error
import qualified Expr
import qualified Lox
import qualified Stmt
import qualified Token

newtype ParserState = ParserState {pTokens :: [Token.Token]} deriving (Eq, Show)

type Parser a = Except.ExceptT Error.Error (State.State ParserState) a

type ExprParser = Parser Expr.Expr

type StmtParser = Parser Stmt.Stmt

parse :: [Token.Token] -> Lox.Result [Stmt.Stmt]
parse tokens = case result of
  Right expr -> Right expr
  Left err -> Left [err]
  where
    (result, _) = flip State.runState (initParser tokens) . Except.runExceptT $ program

initParser :: [Token.Token] -> ParserState
initParser = ParserState

program :: Parser [Stmt.Stmt]
program = do
  atEnd <- State.gets isAtEnd
  if atEnd
    then pure []
    else do
      stmt <- declaration
      stmts <- program
      pure $ stmt : stmts

declaration :: StmtParser
-- TODO synchronize on parse error
declaration = do
  isVariable <- matchToken Token.Var
  if isVariable
    then variableDeclaration
    else statement

variableDeclaration :: StmtParser
variableDeclaration = do
  name <- expect identifier "Expect variable name."
  isAssign <- matchToken Token.Equal
  value <- if isAssign then Just <$> expression else pure Nothing
  expectToken Token.SemiColon "Expect ';' after variable declaration"
  pure $ Stmt.Variable name value

statement :: StmtParser
statement = do
  maybePrint <- State.state $ match isPrint
  case maybePrint of
    Just _ -> printStatement
    Nothing -> expressionStatement

printStatement :: StmtParser
printStatement = do
  expr <- expression
  semi <- State.state $ match isSemicolon
  case semi of
    Just _ -> pure $ Stmt.Print expr
    Nothing -> reportError "Expect ';' after expression."

expressionStatement :: StmtParser
expressionStatement = do
  expr <- expression
  semi <- State.state $ match isSemicolon
  case semi of
    Just _ -> pure $ Stmt.Expression expr
    Nothing -> reportError "Expect ';' after expression."

expression :: ExprParser
expression = equality

equality :: ExprParser
equality = do
  expr <- comparison
  whileEquality expr

whileEquality :: Expr.Expr -> ExprParser
whileEquality expr = do
  maybeOp <- State.state $ match equalityOp
  case maybeOp of
    Just op -> do
      right <- comparison
      whileEquality $ Expr.Binary expr right op
    Nothing -> pure expr

equalityOp :: Token.Token -> Maybe Expr.BinaryOp
equalityOp t
  | Token.tType t == Token.EqualEqual = Just Expr.Equal
  | Token.tType t == Token.BangEqual = Just Expr.NotEqual
  | otherwise = Nothing

comparison :: ExprParser
comparison = do
  expr <- term
  whileComparison expr

whileComparison :: Expr.Expr -> ExprParser
whileComparison expr = do
  maybeOp <- State.state $ match comparisonOp
  case maybeOp of
    Just op -> do
      right <- term
      whileComparison $ Expr.Binary expr right op
    Nothing -> pure expr

comparisonOp :: Token.Token -> Maybe Expr.BinaryOp
comparisonOp t
  | Token.tType t == Token.Greater = Just Expr.Greater
  | Token.tType t == Token.GreaterEqual = Just Expr.GreaterEqual
  | Token.tType t == Token.Less = Just Expr.Less
  | Token.tType t == Token.LessEqual = Just Expr.LessEqual
  | otherwise = Nothing

term :: ExprParser
term = do
  expr <- factor
  whileTerm expr

whileTerm :: Expr.Expr -> ExprParser
whileTerm expr = do
  maybeOp <- State.state $ match termOp
  case maybeOp of
    Just op -> do
      right <- factor
      whileTerm $ Expr.Binary expr right op
    Nothing -> pure expr

termOp :: Token.Token -> Maybe Expr.BinaryOp
termOp t
  | Token.tType t == Token.Minus = Just Expr.Minus
  | Token.tType t == Token.Plus = Just Expr.Plus
  | otherwise = Nothing

factor :: ExprParser
factor = do
  expr <- unary
  whileFactor expr

whileFactor :: Expr.Expr -> ExprParser
whileFactor expr = do
  maybeOp <- State.state $ match factorOp
  case maybeOp of
    Just op -> do
      right <- unary
      whileFactor $ Expr.Binary expr right op
    Nothing -> pure expr

factorOp :: Token.Token -> Maybe Expr.BinaryOp
factorOp t
  | Token.tType t == Token.Slash = Just Expr.Div
  | Token.tType t == Token.Star = Just Expr.Mult
  | otherwise = Nothing

unary :: ExprParser
unary = do
  maybeOp <- State.state $ match unaryOp
  case maybeOp of
    Just op -> do
      expr <- unary
      pure $ Expr.Unary expr op
    Nothing -> primary

unaryOp :: Token.Token -> Maybe Expr.UnaryOp
unaryOp t
  | Token.tType t == Token.Bang = Just Expr.Not
  | Token.tType t == Token.Minus = Just Expr.Neg
  | otherwise = Nothing

primary :: ExprParser
primary = do
  maybeLiteral <- State.state $ match literal
  case maybeLiteral of
    Just l -> pure $ Expr.Literal l
    Nothing -> do
      maybeIdentifier <- State.state $ match identifier
      case maybeIdentifier of
        Just name -> pure $ Expr.Variable name
        Nothing -> grouping

literal :: Token.Token -> Maybe Lox.Value
literal (Token.Token Token.False _ _) = Just $ Lox.Boolean False
literal (Token.Token Token.True _ _) = Just $ Lox.Boolean True
literal (Token.Token Token.Nil _ _) = Just Lox.Nil
literal (Token.Token (Token.Number n) _ _) = Just $ Lox.Number n
literal (Token.Token (Token.String s) _ _) = Just $ Lox.String s
literal _ = Nothing

grouping :: ExprParser
grouping = do
  openParen <- State.state $ match leftParen
  case openParen of
    Just _ -> do
      expr <- expression
      closeParen <- State.state $ match rightParen
      case closeParen of
        Just _ -> pure $ Expr.Grouping expr
        Nothing -> reportError "Expect ')' after expression."
    Nothing -> reportError "Expect expression."

identifier :: Token.Token -> Maybe Text.Text
identifier (Token.Token (Token.Identifier name) _ _) = Just name
identifier _ = Nothing

leftParen :: Token.Token -> Maybe ()
leftParen t
  | Token.tType t == Token.LeftParen = Just ()
  | otherwise = Nothing

rightParen :: Token.Token -> Maybe ()
rightParen t
  | Token.tType t == Token.RightParen = Just ()
  | otherwise = Nothing

isPrint :: Token.Token -> Maybe ()
isPrint t
  | Token.tType t == Token.Print = Just ()
  | otherwise = Nothing

isSemicolon :: Token.Token -> Maybe ()
isSemicolon t
  | Token.tType t == Token.SemiColon = Just ()
  | otherwise = Nothing

matchToken :: Token.TokenType -> Parser Bool
matchToken expected = do
  p <- State.get
  case List.uncons . pTokens $ p of
    Just (t, ts) ->
      if Token.tType t == expected
        then do
          State.put p {pTokens = ts}
          pure True
        else pure False
    Nothing -> pure False

match :: (Token.Token -> Maybe a) -> ParserState -> (Maybe a, ParserState)
match check p =
  case List.uncons . pTokens $ p of
    Just (t, ts) -> case check t of
      Just result -> (Just result, p {pTokens = ts})
      Nothing -> (Nothing, p)
    Nothing -> (Nothing, p)

expectToken :: Token.TokenType -> Text.Text -> Parser ()
expectToken expected errMessage = do
  p <- State.get
  case List.uncons . pTokens $ p of
    Just (t, ts) ->
      if Token.tType t == expected
        then State.put p {pTokens = ts}
        else reportError errMessage
    Nothing -> reportError errMessage

expect :: (Token.Token -> Maybe a) -> Text.Text -> Parser a
expect check msg = do
  p <- State.get
  case List.uncons . pTokens $ p of
    Just (t, ts) -> case check t of
      Just result -> do
        State.put p {pTokens = ts}
        pure result
      Nothing -> reportError msg
    Nothing -> reportError msg

isAtEnd :: ParserState -> Bool
isAtEnd p = case List.uncons . pTokens $ p of
  Just (t, _) -> Token.tType t == Token.EOF
  Nothing -> True

reportError :: Text.Text -> Parser a
reportError e = do
  p <- State.get
  Except.throwError . newError $ p
  where
    newError p = case List.uncons . pTokens $ p of
      Just (t, _) -> Error.Error (Token.tLine t) (Text.concat ["At ", Token.tLexeme t, ": ", e])
      Nothing -> Error.Error 0 e
