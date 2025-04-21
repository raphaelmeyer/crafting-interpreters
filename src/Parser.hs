{-# LANGUAGE OverloadedStrings #-}

module Parser (parse) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Error
import qualified Expr
import qualified Literal
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
  token <- match . anyOf $ [Token.Fun, Token.Var]
  case token of
    Just Token.Fun -> function
    Just Token.Var -> variableDeclaration
    _ -> statement

function :: StmtParser
function = do
  name <- expect identifier "Expect function name."
  expectToken Token.LeftParen "Expect '(' after function name."
  params <- parameters
  if length params > 255
    then reportError "Can't have more than 255 parameters."
    else do
      expectToken Token.LeftBrace "Expect '{' before function body."
      Stmt.Function name params <$> whileBlock

parameters :: Parser [Text.Text]
parameters = do
  isEmpty <- matchToken Token.RightParen
  if isEmpty
    then pure []
    else do
      params <- whileParameters
      expectToken Token.RightParen "Expect ')' after parameters."
      pure params

whileParameters :: Parser [Text.Text]
whileParameters = do
  param <- expect identifier "Expect parameter name."
  isComma <- matchToken Token.Comma
  if isComma
    then do
      params <- whileParameters
      pure $ param : params
    else pure [param]

variableDeclaration :: StmtParser
variableDeclaration = do
  name <- expect identifier "Expect variable name."
  isAssign <- matchToken Token.Equal
  value <- if isAssign then expression else pure $ Expr.Literal Literal.Nil
  expectToken Token.SemiColon "Expect ';' after variable declaration."
  pure $ Stmt.Variable name value

statement :: StmtParser
statement = do
  token <- match . anyOf $ [Token.For, Token.If, Token.Print, Token.Return, Token.While, Token.LeftBrace]
  case token of
    Just Token.For -> forStatement
    Just Token.If -> ifStatement
    Just Token.Print -> printStatement
    Just Token.Return -> returnStatement
    Just Token.While -> whileStatement
    Just Token.LeftBrace -> block
    _ -> expressionStatement

forStatement :: StmtParser
forStatement = do
  expectToken Token.LeftParen "Expect '(' after 'for'."
  initializer <- forInitializer
  condition <- forCondition
  increment <- forIncrement
  wrapInitializer initializer . wrapCondition condition . wrapIncrement increment <$> statement
  where
    wrapIncrement maybeIncrement body = case maybeIncrement of
      Just increment -> Stmt.Block [body, Stmt.Expression increment]
      Nothing -> body
    wrapCondition = Stmt.While
    wrapInitializer maybeInitializer body = case maybeInitializer of
      Just initializer -> Stmt.Block [initializer, body]
      Nothing -> body

forInitializer :: Parser (Maybe Stmt.Stmt)
forInitializer = do
  token <- match . anyOf $ [Token.SemiColon, Token.Var]
  case token of
    Just Token.SemiColon -> pure Nothing
    Just Token.Var -> Just <$> variableDeclaration
    _ -> Just <$> expressionStatement

forCondition :: ExprParser
forCondition = do
  noCondition <- matchToken Token.SemiColon
  if noCondition
    then pure $ Expr.Literal (Literal.Boolean True)
    else do
      condition <- expression
      expectToken Token.SemiColon "Expect ';' after loop condition."
      pure condition

forIncrement :: Parser (Maybe Expr.Expr)
forIncrement = do
  noIncrement <- matchToken Token.RightParen
  if noIncrement
    then pure Nothing
    else do
      increment <- expression
      expectToken Token.RightParen "Expect ')' after for clauses."
      pure $ Just increment

ifStatement :: StmtParser
ifStatement = do
  expectToken Token.LeftParen "Expect '(' after 'if'."
  condition <- expression
  expectToken Token.RightParen "Expect ')' after if condition."
  thenStmt <- statement
  isElse <- matchToken Token.Else
  elseStmt <-
    if isElse
      then Just <$> statement
      else pure Nothing
  pure $ Stmt.If condition thenStmt elseStmt

printStatement :: StmtParser
printStatement = do
  expr <- expression
  expectToken Token.SemiColon "Expect ';' after expression."
  pure $ Stmt.Print expr

returnStatement :: StmtParser
returnStatement = do
  isEmpty <- matchToken Token.SemiColon
  if isEmpty
    then pure . Stmt.Return . Expr.Literal $ Literal.Nil
    else do
      value <- expression
      expectToken Token.SemiColon "Expect ';' after return value."
      pure $ Stmt.Return value

whileStatement :: StmtParser
whileStatement = do
  expectToken Token.LeftParen "Expect '(' after 'while'."
  condition <- expression
  expectToken Token.RightParen "Expect ')' after condition."
  Stmt.While condition <$> statement

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
  expr <- logicalOr
  isAssign <- matchToken Token.Equal
  if isAssign
    then do
      value <- assignment
      case expr of
        (Expr.Variable name) -> pure $ Expr.Assign name value
        _ -> reportError "Invalid assignment target."
    else pure expr

logicalOr :: ExprParser
logicalOr = do
  expr <- logicalAnd
  whileLogicalOr expr

whileLogicalOr :: Expr.Expr -> ExprParser
whileLogicalOr expr = do
  isOr <- matchToken Token.Or
  if isOr
    then do
      right <- logicalAnd
      whileLogicalOr $ Expr.Logical Expr.Or expr right
    else pure expr

logicalAnd :: ExprParser
logicalAnd = do
  expr <- equality
  whileLogicalAnd expr

whileLogicalAnd :: Expr.Expr -> ExprParser
whileLogicalAnd expr = do
  isAnd <- matchToken Token.And
  if isAnd
    then do
      right <- equality
      whileLogicalAnd $ Expr.Logical Expr.And expr right
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
      whileEquality $ Expr.Binary op expr right
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
      whileComparison $ Expr.Binary op expr right
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
      whileTerm $ Expr.Binary op expr right
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
      whileFactor $ Expr.Binary op expr right
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
      Expr.Unary op <$> unary
    Nothing -> call

unaryOp :: Token.Token -> Maybe Expr.UnaryOp
unaryOp t
  | Token.tType t == Token.Bang = Just Expr.Not
  | Token.tType t == Token.Minus = Just Expr.Neg
  | otherwise = Nothing

call :: ExprParser
call = do
  expr <- primary
  whileCall expr

whileCall :: Expr.Expr -> ExprParser
whileCall expr = do
  isParen <- matchToken Token.LeftParen
  if isParen
    then do
      args <- arguments
      if length args > 255
        then reportError "Can't have more than 255 arguments."
        else whileCall $ Expr.Call expr args
    else pure expr

arguments :: Parser [Expr.Expr]
arguments = do
  isEmpty <- matchToken Token.RightParen
  if isEmpty
    then pure []
    else do
      args <- whileArguments
      expectToken Token.RightParen "Expect ')' after arguments."
      pure args

whileArguments :: Parser [Expr.Expr]
whileArguments = do
  arg <- expression
  isComma <- matchToken Token.Comma
  if isComma
    then do
      args <- whileArguments
      pure $ arg : args
    else pure [arg]

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

literal :: Token.Token -> Maybe Literal.Value
literal t = case Token.tType t of
  Token.False -> Just $ Literal.Boolean False
  Token.True -> Just $ Literal.Boolean True
  Token.Nil -> Just Literal.Nil
  (Token.Number n) -> Just $ Literal.Number n
  (Token.String s) -> Just $ Literal.String s
  _ -> Nothing

identifier :: Token.Token -> Maybe Text.Text
identifier (Token.Token (Token.Identifier name) _ _) = Just name
identifier _ = Nothing

isToken :: Token.TokenType -> Token.Token -> Maybe ()
isToken tType token =
  if tType == Token.tType token
    then Just ()
    else Nothing

anyOf :: [Token.TokenType] -> Token.Token -> Maybe Token.TokenType
anyOf expected token =
  if tType `elem` expected
    then Just tType
    else Nothing
  where
    tType = Token.tType token

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
      Just (t, _) -> Error.ParseError (Token.tLine t) (Token.tLexeme t) e
      Nothing -> Error.ParseError 0 "EOF" e

synchronize :: State.State ParserState ()
synchronize = do
  p <- State.get
  case List.uncons . pTokens $ p of
    Just (t, ts) -> do
      State.put p {pTokens = if isSemiColon t then ts else sync (t : ts)}
    Nothing -> pure ()
  where
    sync = dropWhile (not . syncPoint)
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
