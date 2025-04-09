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
parse tokens = State.evalState program (initParser tokens)

initParser :: [Token.Token] -> ParserState
initParser = ParserState

program :: State.State ParserState (Lox.Result [Stmt.Stmt])
program = do
  atEnd <- State.gets isAtEnd
  if atEnd
    then pure $ Right []
    else do
      result <- Except.runExceptT declaration
      case result of
        Right stmt -> do
          addStmt stmt <$> program
        Left err -> do
          synchronize
          addError err <$> program

addStmt :: Stmt.Stmt -> Lox.Result [Stmt.Stmt] -> Lox.Result [Stmt.Stmt]
addStmt stmt result = case result of
  Right stmts -> Right $ stmt : stmts
  Left errs -> Left errs

addError :: Error.Error -> Lox.Result [Stmt.Stmt] -> Lox.Result [Stmt.Stmt]
addError err result = case result of
  Right _ -> Left [err]
  Left errs -> Left $ err : errs

declaration :: StmtParser
declaration = do
  isVariable <- matchToken Token.Var
  if isVariable
    then variableDeclaration
    else statement

variableDeclaration :: StmtParser
variableDeclaration = do
  name <- expect identifier "Expect variable name."
  isAssign <- matchToken Token.Equal
  value <- if isAssign then expression else pure $ Expr.Literal Lox.Nil
  expectToken Token.SemiColon "Expect ';' after variable declaration."
  pure $ Stmt.Variable name value

statement :: StmtParser
statement = do
  isPrint <- matchToken Token.Print
  if isPrint
    then printStatement
    else do
      isBlock <- matchToken Token.LeftBrace
      if isBlock
        then block
        else expressionStatement

printStatement :: StmtParser
printStatement = do
  expr <- expression
  expectToken Token.SemiColon "Expect ';' after expression."
  pure $ Stmt.Print expr

expressionStatement :: StmtParser
expressionStatement = do
  expr <- expression
  expectToken Token.SemiColon "Expect ';' after expression."
  pure $ Stmt.Expression expr

block :: StmtParser
block = Stmt.Block <$> whileBlock

whileBlock :: Parser [Stmt.Stmt]
whileBlock = do
  isClosing <- matchToken Token.RightBrace
  if isClosing
    then pure []
    else do
      atEnd <- State.gets isAtEnd
      if atEnd
        then reportError "Expect '}' after block."
        else do
          stmt <- declaration
          stmts <- whileBlock
          pure $ stmt : stmts

expression :: ExprParser
expression = assignment

assignment :: ExprParser
assignment = do
  expr <- equality
  isAssign <- matchToken Token.Equal
  if isAssign
    then do
      value <- assignment
      case expr of
        (Expr.Variable name) -> pure $ Expr.Assign name value
        _ -> reportError "Invalid assignment target."
    else pure expr

equality :: ExprParser
equality = do
  expr <- comparison
  whileEquality expr

whileEquality :: Expr.Expr -> ExprParser
whileEquality expr = do
  maybeOp <- match equalityOp
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
  maybeOp <- match comparisonOp
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
  maybeOp <- match termOp
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
  maybeOp <- match factorOp
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
  maybeOp <- match unaryOp
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
  maybeLiteral <- match literal
  case maybeLiteral of
    Just l -> pure $ Expr.Literal l
    Nothing -> do
      maybeIdentifier <- match identifier
      case maybeIdentifier of
        Just name -> pure $ Expr.Variable name
        Nothing -> grouping

grouping :: ExprParser
grouping = do
  expectToken Token.LeftParen "Expect expression."
  expr <- expression
  expectToken Token.RightParen "Expect ')' after expression."
  pure $ Expr.Grouping expr

literal :: Token.Token -> Maybe Lox.Value
literal t = case Token.tType t of
  Token.False -> Just $ Lox.Boolean False
  Token.True -> Just $ Lox.Boolean True
  Token.Nil -> Just Lox.Nil
  (Token.Number n) -> Just $ Lox.Number n
  (Token.String s) -> Just $ Lox.String s
  _ -> Nothing

identifier :: Token.Token -> Maybe Text.Text
identifier (Token.Token (Token.Identifier name) _ _) = Just name
identifier _ = Nothing

isToken :: Token.TokenType -> Token.Token -> Maybe ()
isToken tType token =
  if tType == Token.tType token
    then Just ()
    else Nothing

match :: (Token.Token -> Maybe a) -> Parser (Maybe a)
match = advance

expect :: (Token.Token -> Maybe a) -> Text.Text -> Parser a
expect check msg = do
  m <- advance check
  case m of
    Just result -> pure result
    Nothing -> reportError msg

matchToken :: Token.TokenType -> Parser Bool
matchToken expected = do
  m <- advance $ isToken expected
  pure $ case m of
    Just _ -> True
    Nothing -> False

expectToken :: Token.TokenType -> Text.Text -> Parser ()
expectToken expected msg = do
  m <- advance $ isToken expected
  case m of
    Just _ -> pure ()
    Nothing -> reportError msg

advance :: (Token.Token -> Maybe a) -> Parser (Maybe a)
advance check = do
  next <- List.uncons . pTokens <$> State.get
  case next of
    Just (t, ts) -> case check t of
      Just result -> do
        State.modify $ \p -> p {pTokens = ts}
        pure $ Just result
      Nothing -> pure Nothing
    Nothing -> pure Nothing

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

synchronize :: State.State ParserState ()
synchronize = do
  State.modify $ \p -> p {pTokens = sync $ pTokens p}
  where
    sync = dropWhile isSemiColon . dropWhile (not . syncPoint)
    isSemiColon = (== Token.SemiColon) . Token.tType

syncPoint :: Token.Token -> Bool
syncPoint t = case Token.tType t of
  Token.SemiColon -> True
  Token.Class -> True
  Token.Fun -> True
  Token.Var -> True
  Token.For -> True
  Token.If -> True
  Token.While -> True
  Token.Print -> True
  Token.Return -> True
  _ -> False
