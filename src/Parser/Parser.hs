{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (parse) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Lox
import qualified Parser.Expr as Expr
import qualified Parser.Literal as Literal
import qualified Parser.Stmt as Stmt
import qualified Scanner.Token as Token

data ParserState = ParserState
  { pTokens :: [Token.Token],
    pErrors :: [Lox.Error]
  }
  deriving (Eq, Show)

type Parser a = Except.ExceptT Lox.Error (State.State ParserState) a

type ExprParser = Parser Expr.Expr

type StmtParser = Parser Stmt.Stmt

parse :: [Token.Token] -> Lox.Result [Stmt.Stmt]
parse tokens = case pErrors p of
  [] -> Right stmts
  errs -> Left errs
  where
    (stmts, p) = State.runState program (initParser tokens)

initParser :: [Token.Token] -> ParserState
initParser tokens = ParserState tokens []

program :: State.State ParserState [Stmt.Stmt]
program = do
  atEnd <- State.gets isAtEnd
  if atEnd
    then pure []
    else do
      result <- Except.runExceptT declaration
      case result of
        Right stmt -> do
          stmts <- program
          pure $ stmt : stmts
        Left err -> do
          p <- State.get
          State.put p {pErrors = err : pErrors p}
          synchronize
          program

declaration :: StmtParser
declaration = do
  token <- match . anyOf $ [Token.Fun, Token.Var]
  case token of
    Just Token.Fun -> function
    Just Token.Var -> variableDeclaration
    _ -> statement

function :: StmtParser
function = do
  (name, _) <- expect identifier "Expect function name."
  expectToken Token.LeftParen "Expect '(' after function name."
  params <- parameters
  expectToken Token.LeftBrace "Expect '{' before function body."
  Stmt.Function name params <$> whileBlock

parameters :: Parser [Expr.Identifier]
parameters = do
  isEmpty <- matchToken Token.RightParen
  if isEmpty
    then pure []
    else do
      params <- whileParameters 0
      expectToken Token.RightParen "Expect ')' after parameters."
      pure params

whileParameters :: Int -> Parser [Expr.Identifier]
whileParameters count = do
  Monad.when (count >= 255) $
    errorCurrentToken "Can't have more than 255 parameters." >>= report
  (param, _) <- expect identifier "Expect parameter name."
  isComma <- matchToken Token.Comma
  if isComma
    then do
      params <- whileParameters (count + 1)
      pure $ param : params
    else pure [param]

variableDeclaration :: StmtParser
variableDeclaration = do
  (name, loc) <- expect identifier "Expect variable name."
  isAssign <- matchToken Token.Equal
  value <- if isAssign then expression else pure $ Expr.Expr (Expr.Literal Literal.Nil) loc
  expectToken Token.SemiColon "Expect ';' after variable declaration."
  pure $ Stmt.Variable name value

statement :: StmtParser
statement = do
  token <- match . anyOf $ [Token.Break, Token.For, Token.If, Token.Print, Token.Return, Token.While, Token.LeftBrace]
  case token of
    Just Token.Break -> breakStatement
    Just Token.For -> forStatement
    Just Token.If -> ifStatement
    Just Token.Print -> printStatement
    Just Token.Return -> returnStatement
    Just Token.While -> whileStatement
    Just Token.LeftBrace -> block
    _ -> expressionStatement

breakStatement :: StmtParser
breakStatement = do
  expectToken Token.SemiColon "Expect ';' after 'break'."
  pure Stmt.Break

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
  noCondition <- matchTokenAt Token.SemiColon
  case noCondition of
    Just loc -> pure $ Expr.Expr (Expr.Literal (Literal.Boolean True)) loc
    Nothing -> do
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
  isEmpty <- matchTokenAt Token.SemiColon
  case isEmpty of
    Just loc -> pure . Stmt.Return $ Expr.Expr (Expr.Literal Literal.Nil) loc
    Nothing -> do
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
        then errorCurrentToken "Expect '}' after block." >>= throw
        else do
          stmt <- declaration
          stmts <- whileBlock
          pure $ stmt : stmts

expression :: ExprParser
expression = assignment

assignment :: ExprParser
assignment = do
  expr <- logicalOr
  maybeAssign <- match isEqual
  case maybeAssign of
    Just assign -> do
      value <- assignment
      case expr of
        Expr.Expr (Expr.Variable variable) loc -> pure $ Expr.Expr (Expr.Assign variable value) loc
        _ -> errorWithToken assign "Invalid assignment target." >>= throw
    Nothing -> pure expr

isEqual :: Token.Token -> Maybe Token.Token
isEqual t =
  if Token.tType t == Token.Equal
    then Just t
    else Nothing

logicalOr :: ExprParser
logicalOr = do
  expr <- logicalAnd
  whileLogicalOr expr

whileLogicalOr :: Expr.Expr -> ExprParser
whileLogicalOr expr = do
  maybeOr <- match $ mkOperator [(Token.Or, Expr.Or)]
  case maybeOr of
    Just (op, loc) -> do
      right <- logicalAnd
      whileLogicalOr $ Expr.Expr (Expr.Logical op expr right) loc
    Nothing -> pure expr

logicalAnd :: ExprParser
logicalAnd = do
  expr <- equality
  whileLogicalAnd expr

whileLogicalAnd :: Expr.Expr -> ExprParser
whileLogicalAnd expr = do
  maybeAnd <- match $ mkOperator [(Token.And, Expr.And)]
  case maybeAnd of
    Just (op, loc) -> do
      right <- equality
      whileLogicalAnd $ Expr.Expr (Expr.Logical op expr right) loc
    Nothing -> pure expr

equality :: ExprParser
equality = do
  expr <- comparison
  whileEquality expr

whileEquality :: Expr.Expr -> ExprParser
whileEquality expr = do
  maybeOp <- match equalityOp
  case maybeOp of
    Just (op, loc) -> do
      right <- comparison
      whileEquality $ Expr.Expr (Expr.Binary op expr right) loc
    Nothing -> pure expr

equalityOp :: Token.Token -> Maybe (Expr.BinaryOp, Expr.Location)
equalityOp =
  mkOperator
    [ (Token.EqualEqual, Expr.Equal),
      (Token.BangEqual, Expr.NotEqual)
    ]

comparison :: ExprParser
comparison = do
  expr <- term
  whileComparison expr

whileComparison :: Expr.Expr -> ExprParser
whileComparison expr = do
  maybeOp <- match comparisonOp
  case maybeOp of
    Just (op, loc) -> do
      right <- term
      whileComparison $ Expr.Expr (Expr.Binary op expr right) loc
    Nothing -> pure expr

comparisonOp :: Token.Token -> Maybe (Expr.BinaryOp, Expr.Location)
comparisonOp =
  mkOperator
    [ (Token.Greater, Expr.Greater),
      (Token.GreaterEqual, Expr.GreaterEqual),
      (Token.Less, Expr.Less),
      (Token.LessEqual, Expr.LessEqual)
    ]

term :: ExprParser
term = do
  expr <- factor
  whileTerm expr

whileTerm :: Expr.Expr -> ExprParser
whileTerm expr = do
  maybeOp <- match termOp
  case maybeOp of
    Just (op, loc) -> do
      right <- factor
      whileTerm $ Expr.Expr (Expr.Binary op expr right) loc
    Nothing -> pure expr

termOp :: Token.Token -> Maybe (Expr.BinaryOp, Expr.Location)
termOp =
  mkOperator
    [ (Token.Minus, Expr.Minus),
      (Token.Plus, Expr.Plus)
    ]

factor :: ExprParser
factor = do
  expr <- unary
  whileFactor expr

whileFactor :: Expr.Expr -> ExprParser
whileFactor expr = do
  maybeOp <- match factorOp
  case maybeOp of
    Just (op, loc) -> do
      right <- unary
      whileFactor $ Expr.Expr (Expr.Binary op expr right) loc
    Nothing -> pure expr

factorOp :: Token.Token -> Maybe (Expr.BinaryOp, Expr.Location)
factorOp =
  mkOperator
    [ (Token.Slash, Expr.Div),
      (Token.Star, Expr.Mult)
    ]

unary :: ExprParser
unary = do
  maybeOp <- match unaryOp
  case maybeOp of
    Just (op, loc) -> do
      expr <- unary
      pure $ Expr.Expr (Expr.Unary op expr) loc
    Nothing -> call

unaryOp :: Token.Token -> Maybe (Expr.UnaryOp, Expr.Location)
unaryOp =
  mkOperator
    [ (Token.Bang, Expr.Not),
      (Token.Minus, Expr.Neg)
    ]

mkOperator :: [(Token.TokenType, a)] -> Token.Token -> Maybe (a, Expr.Location)
mkOperator ((tType, op) : types) t =
  if tType == Token.tType t
    then Just (op, Token.tLine t)
    else mkOperator types t
mkOperator [] _ = Nothing

call :: ExprParser
call = do
  expr <- primary
  whileCall expr

whileCall :: Expr.Expr -> ExprParser
whileCall expr = do
  isParen <- matchToken Token.LeftParen
  if isParen
    then do
      (args, loc) <- arguments
      whileCall $ Expr.Expr (Expr.Call expr args) loc
    else pure expr

arguments :: Parser ([Expr.Expr], Expr.Location)
arguments = do
  maybeParen <- matchTokenAt Token.RightParen
  case maybeParen of
    Just loc -> pure ([], loc)
    Nothing -> do
      args <- whileArguments 0
      location <- expectTokenAt Token.RightParen "Expect ')' after arguments."
      pure (args, location)

whileArguments :: Int -> Parser [Expr.Expr]
whileArguments count = do
  Monad.when (count >= 255) $
    errorCurrentToken "Can't have more than 255 arguments." >>= report
  arg <- expression
  isComma <- matchToken Token.Comma
  if isComma
    then do
      args <- whileArguments (count + 1)
      pure $ arg : args
    else pure [arg]

primary :: ExprParser
primary = do
  maybeLiteral <- match literal
  case maybeLiteral of
    Just (l, loc) -> pure $ Expr.Expr (Expr.Literal l) loc
    Nothing -> do
      maybeIdentifier <- match identifier
      case maybeIdentifier of
        Just (name, loc) -> pure $ Expr.Expr (Expr.Variable name) loc
        Nothing -> grouping

grouping :: ExprParser
grouping = do
  loc <- expectTokenAt Token.LeftParen "Expect expression."
  expr <- expression
  expectToken Token.RightParen "Expect ')' after expression."
  pure $ Expr.Expr (Expr.Grouping expr) loc

literal :: Token.Token -> Maybe (Literal.Value, Expr.Location)
literal t = case Token.tType t of
  Token.False -> mkLiteral $ Literal.Boolean False
  Token.True -> mkLiteral $ Literal.Boolean True
  Token.Nil -> mkLiteral Literal.Nil
  (Token.Number n) -> mkLiteral $ Literal.Number n
  (Token.String s) -> mkLiteral $ Literal.String s
  _ -> Nothing
  where
    mkLiteral l = Just (l, Token.tLine t)

identifier :: Token.Token -> Maybe (Expr.Identifier, Expr.Location)
identifier (Token.Token (Token.Identifier name) _ line) = Just (name, line)
identifier _ = Nothing

isToken :: Token.TokenType -> Token.Token -> Maybe Token.Token
isToken tType token =
  if tType == Token.tType token
    then Just token
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
    Nothing -> errorCurrentToken msg >>= throw

matchToken :: Token.TokenType -> Parser Bool
matchToken expected = do
  m <- matchTokenAt expected
  pure $ Maybe.isJust m

expectToken :: Token.TokenType -> Text.Text -> Parser ()
expectToken expected msg = do
  Monad.void $ expectTokenAt expected msg

matchTokenAt :: Token.TokenType -> Parser (Maybe Expr.Location)
matchTokenAt expected = do
  m <- advance $ isToken expected
  pure $ fmap Token.tLine m

expectTokenAt :: Token.TokenType -> Text.Text -> Parser Expr.Location
expectTokenAt expected msg = do
  m <- advance $ isToken expected
  case m of
    Just t -> pure $ Token.tLine t
    Nothing -> errorCurrentToken msg >>= throw

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

throw :: Lox.Error -> Parser a
throw = Except.throwError

report :: Lox.Error -> Parser ()
report err = do
  p <- State.get
  State.put p {pErrors = err : pErrors p}
  pure ()

errorCurrentToken :: Text.Text -> Parser Lox.Error
errorCurrentToken msg = do
  p <- State.get
  case List.uncons . pTokens $ p of
    Just (t, _) -> errorWithToken t msg
    Nothing -> pure $ Lox.ParseError 0 "EOF" msg

errorWithToken :: Token.Token -> Text.Text -> Parser Lox.Error
errorWithToken t = errorAt (Token.tLine t) (Token.tLexeme t)

errorAt :: Expr.Location -> Text.Text -> Text.Text -> Parser Lox.Error
errorAt loc lexeme msg = pure $ Lox.ParseError loc lexeme msg

synchronize :: State.State ParserState ()
synchronize = do
  p <- State.get
  case List.uncons $ pTokens p of
    Just (_, ts) -> State.put p {pTokens = consumeUntilSync ts}
    Nothing -> pure ()

consumeUntilSync :: [Token.Token] -> [Token.Token]
consumeUntilSync [] = []
consumeUntilSync (t : ts)
  | syncPoint t = t : ts
  | isSemiColon t = ts
  | isEnd t = [t]
  | otherwise = consumeUntilSync ts
  where
    isSemiColon = (== Token.SemiColon) . Token.tType
    isEnd = (== Token.EOF) . Token.tType

syncPoint :: Token.Token -> Bool
syncPoint t = case Token.tType t of
  Token.Class -> True
  Token.Fun -> True
  Token.Var -> True
  Token.For -> True
  Token.If -> True
  Token.While -> True
  Token.Print -> True
  Token.Return -> True
  _ -> False
