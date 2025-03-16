module Parser (parse) where

import qualified Control.Monad.State.Strict as State
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
  token <- match [Token.BangEqual, Token.EqualEqual]
  case token of
    Just t -> do
      right <- comparison
      whileEquality $ Expr.Binary expr right (mapToken . Token.tType $ t)
    Nothing -> pure expr
  where
    mapToken Token.BangEqual = Expr.NotEqual
    mapToken Token.EqualEqual = Expr.Equal
    mapToken _ = undefined

comparison :: State.State Parser Expr.Expr
comparison = term

term :: State.State Parser Expr.Expr
term = factor

factor :: State.State Parser Expr.Expr
factor = unary

unary :: State.State Parser Expr.Expr
unary = primary

primary :: State.State Parser Expr.Expr
primary = do
  s <- State.get
  case pTokens s of
    (Token.Token (Token.Number n) _ _) : _ -> do
      _ <- advance
      pure . Expr.Literal $ Expr.Number n
    _ -> undefined

match :: [Token.TokenType] -> State.State Parser (Maybe Token.Token)
match expected = do
  s <- State.get
  case pTokens s of
    [] -> pure Nothing
    (t : _) ->
      if Token.tType t `elem` expected
        then do advance
        else pure Nothing

advance :: State.State Parser (Maybe Token.Token)
advance = do
  s <- State.get
  case pTokens s of
    [] -> pure Nothing
    t : ts -> do
      State.put s {pTokens = ts}
      pure $ Just t
