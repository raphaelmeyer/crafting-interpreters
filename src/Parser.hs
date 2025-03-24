{-# LANGUAGE OverloadedStrings #-}

module Parser (parse) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Error
import qualified Expr
import qualified Lox
import qualified Token

newtype ParserState = ParserState {pTokens :: [Token.Token]} deriving (Eq, Show)

type Parser = Except.ExceptT Error.Error (State.State ParserState) Expr.Expr

parse :: [Token.Token] -> Lox.Result Expr.Expr
parse tokens = case result of
  Right expr -> Right expr
  Left err -> Left [err]
  where
    (result, _) = flip State.runState (initParser tokens) . Except.runExceptT $ expression

initParser :: [Token.Token] -> ParserState
initParser = ParserState

expression :: Parser
expression = equality

equality :: Parser
equality = do
  expr <- comparison
  whileEquality expr

whileEquality :: Expr.Expr -> Parser
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

comparison :: Parser
comparison = do
  expr <- term
  whileComparison expr

whileComparison :: Expr.Expr -> Parser
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

term :: Parser
term = do
  expr <- factor
  whileTerm expr

whileTerm :: Expr.Expr -> Parser
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

factor :: Parser
factor = do
  expr <- unary
  whileFactor expr

whileFactor :: Expr.Expr -> Parser
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

unary :: Parser
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

primary :: Parser
primary = do
  maybeLiteral <- State.state $ match literal
  case maybeLiteral of
    Just l -> pure $ Expr.Literal l
    Nothing -> grouping

literal :: Token.Token -> Maybe Lox.Value
literal (Token.Token Token.False _ _) = Just $ Lox.Boolean False
literal (Token.Token Token.True _ _) = Just $ Lox.Boolean True
literal (Token.Token Token.Nil _ _) = Just $ Lox.Nil
literal (Token.Token (Token.Number n) _ _) = Just $ Lox.Number n
literal (Token.Token (Token.String s) _ _) = Just $ Lox.String s
literal _ = Nothing

grouping :: Parser
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

leftParen :: Token.Token -> Maybe ()
leftParen t
  | Token.tType t == Token.LeftParen = Just ()
  | otherwise = Nothing

rightParen :: Token.Token -> Maybe ()
rightParen t
  | Token.tType t == Token.RightParen = Just ()
  | otherwise = Nothing

match :: (Token.Token -> Maybe a) -> ParserState -> (Maybe a, ParserState)
match check p = do
  case List.uncons . pTokens $ p of
    Just (t, ts) -> case check t of
      Just result -> (Just result, p {pTokens = ts})
      Nothing -> (Nothing, p)
    Nothing -> (Nothing, p)

reportError :: Text.Text -> Parser
reportError e = do
  p <- State.get
  Except.throwError . newError $ p
  where
    newError p = case List.uncons . pTokens $ p of
      Just (t, _) -> Error.Error (Token.tLine t) (Text.concat ["At ", Token.tLexeme t, ": ", e])
      Nothing -> Error.Error 0 e
