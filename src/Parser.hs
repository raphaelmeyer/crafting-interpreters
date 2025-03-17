module Parser (parse) where

import qualified Control.Monad.State.Strict as State
import qualified Data.List as List
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

parse :: [Token.Token] -> Expr.Expr
parse tokens = fst $ State.runState expression (initParser tokens)

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
unary = primary

primary :: State.State Parser Expr.Expr
primary = do
  s <- State.get
  case pTokens s of
    (Token.Token (Token.Number n) _ _) : _ -> do
      State.modify skip
      pure . Expr.Literal $ Expr.Number n
    _ -> undefined

match :: (Token.Token -> Maybe a) -> Parser -> (Maybe a, Parser)
match check p = do
  case List.uncons . pTokens $ p of
    Just (t, ts) -> case check t of
      Just result -> (Just result, p {pTokens = ts})
      Nothing -> (Nothing, p)
    Nothing -> (Nothing, p)

skip :: Parser -> Parser
skip p = if null . pTokens $ p then p else p {pTokens = tail . pTokens $ p}
