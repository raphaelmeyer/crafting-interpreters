{-# LANGUAGE OverloadedStrings #-}

module Parser (parse) where

import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Error
import qualified Expr
import qualified Token

data Parser = Parser
  { pTokens :: [Token.Token],
    pErrors :: [Error.Error]
  }
  deriving (Eq, Show)

initParser :: [Token.Token] -> Parser
initParser tokens = Parser tokens []

-- revisit 6.3.3 later and properly return and recover from errors
parse :: [Token.Token] -> Either [Error.Error] Expr.Expr
parse tokens = case pErrors parser of
  [] -> Right expr
  errors -> Left errors
  where
    (expr, parser) = State.runState expression (initParser tokens)

expression :: State.State Parser Expr.Expr
expression = equality

equality :: State.State Parser Expr.Expr
equality = do
  expr <- comparison
  whileEquality expr

whileEquality :: Expr.Expr -> State.State Parser Expr.Expr
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

comparison :: State.State Parser Expr.Expr
comparison = do
  expr <- term
  whileComparison expr

whileComparison :: Expr.Expr -> State.State Parser Expr.Expr
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

term :: State.State Parser Expr.Expr
term = do
  expr <- factor
  whileTerm expr

whileTerm :: Expr.Expr -> State.State Parser Expr.Expr
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

factor :: State.State Parser Expr.Expr
factor = do
  expr <- unary
  whileFactor expr

whileFactor :: Expr.Expr -> State.State Parser Expr.Expr
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

unary :: State.State Parser Expr.Expr
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

primary :: State.State Parser Expr.Expr
primary = do
  maybeLiteral <- State.state $ match literal
  case maybeLiteral of
    Just l -> pure $ Expr.Literal l
    Nothing -> grouping

literal :: Token.Token -> Maybe Expr.LiteralValue
literal (Token.Token Token.False _ _) = Just $ Expr.Boolean False
literal (Token.Token Token.True _ _) = Just $ Expr.Boolean True
literal (Token.Token Token.Nil _ _) = Just $ Expr.Nil
literal (Token.Token (Token.Number n) _ _) = Just $ Expr.Number n
literal (Token.Token (Token.String s) _ _) = Just $ Expr.String s
literal _ = Nothing

grouping :: State.State Parser Expr.Expr
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

match :: (Token.Token -> Maybe a) -> Parser -> (Maybe a, Parser)
match check p = do
  case List.uncons . pTokens $ p of
    Just (t, ts) -> case check t of
      Just result -> (Just result, p {pTokens = ts})
      Nothing -> (Nothing, p)
    Nothing -> (Nothing, p)

reportError :: Text.Text -> State.State Parser Expr.Expr
reportError e = do
  State.modify (addError e)
  undefined

addError :: Text.Text -> Parser -> Parser
addError e p =
  p {pErrors = newError : pErrors p}
  where
    newError = case List.uncons . pTokens $ p of
      Just (t, _) -> Error.Error (Token.tLine t) (Text.concat ["At ", Token.tLexeme t, ": ", e])
      Nothing -> Error.Error 0 e
