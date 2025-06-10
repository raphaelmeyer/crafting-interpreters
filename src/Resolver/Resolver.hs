{-# LANGUAGE OverloadedStrings #-}

module Resolver.Resolver (resolve) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lox
import qualified Parser.Expr as Expr
import qualified Parser.Stmt as Stmt

type Scope = Map.Map Text.Text Bool

data ResolverState = ResolverState
  { rScopes :: [Scope],
    rErrors :: [Lox.Error]
  }

type Resolver a = State.State ResolverState a

resolve :: [Stmt.Stmt] -> Lox.Result [Stmt.Stmt]
resolve stmts = case rErrors s of
  [] -> Right resolved
  errors -> Left errors
  where
    (resolved, s) = State.runState (program stmts) (ResolverState [] [])

program :: [Stmt.Stmt] -> Resolver [Stmt.Stmt]
program = mapM statement

statement :: Stmt.Stmt -> Resolver Stmt.Stmt
statement (Stmt.Block block) = do
  beginScope
  resolved <- program block
  endScope
  pure $ Stmt.Block resolved
statement (Stmt.Variable name initializer) = do
  declare name
  resolved <- expression initializer
  define name
  pure $ Stmt.Variable name resolved
statement (Stmt.Function name params body) = do
  declare name
  define name
  resolveFunction name params body
statement (Stmt.Expression expr) = Stmt.Expression <$> expression expr
statement (Stmt.If condition thenBranch elseBranch) = do
  resCond <- expression condition
  resThen <- statement thenBranch
  resElse <- case elseBranch of
    Just stmt -> Just <$> statement stmt
    Nothing -> pure Nothing
  pure $ Stmt.If resCond resThen resElse
statement (Stmt.Print expr) = do
  resolved <- expression expr
  pure $ Stmt.Print resolved
statement (Stmt.Return expr) = Stmt.Return <$> expression expr
statement (Stmt.While condition body) = do
  resCond <- expression condition
  resBody <- statement body
  pure $ Stmt.While resCond resBody
statement Stmt.Break = pure Stmt.Break

expression :: Expr.Expr -> Resolver Expr.Expr
expression (Expr.Expr (Expr.Variable name _) loc) = do
  checkNoSelfReference name
  d <- resolveLocal name
  pure (Expr.Expr (Expr.Variable name d) loc)
expression (Expr.Expr (Expr.Assign name expr _) loc) = do
  resolved <- expression expr
  d <- resolveLocal name
  pure $ Expr.Expr (Expr.Assign name resolved d) loc
expression (Expr.Expr (Expr.Binary op left right) loc) = do
  resLeft <- expression left
  resRight <- expression right
  pure $ Expr.Expr (Expr.Binary op resLeft resRight) loc
expression (Expr.Expr (Expr.Call callee args) loc) = do
  resCallee <- expression callee
  resArgs <- mapM expression args
  pure $ Expr.Expr (Expr.Call resCallee resArgs) loc
expression (Expr.Expr (Expr.Grouping expr) loc) = do
  resolved <- expression expr
  pure $ Expr.Expr (Expr.Grouping resolved) loc
expression (Expr.Expr (Expr.Literal literal) loc) = pure $ Expr.Expr (Expr.Literal literal) loc
expression (Expr.Expr (Expr.Logical op left right) loc) = do
  resLeft <- expression left
  resRight <- expression right
  pure $ Expr.Expr (Expr.Logical op resLeft resRight) loc
expression (Expr.Expr (Expr.Unary op expr) loc) = do
  resolved <- expression expr
  pure $ Expr.Expr (Expr.Unary op resolved) loc

resolveFunction :: Expr.Identifier -> [Expr.Identifier] -> [Stmt.Stmt] -> Resolver Stmt.Stmt
resolveFunction name params body = do
  beginScope
  mapM_ (\p -> declare p >> define p) params
  resolved <- program body
  endScope
  pure $ Stmt.Function name params resolved

checkNoSelfReference :: Expr.Identifier -> Resolver ()
checkNoSelfReference name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : _) -> case Map.lookup (Expr.idName name) scope of
      Just False -> reportError name "Can't read local variable in its own initializer."
      _ -> pure ()

resolveLocal :: Expr.Identifier -> Resolver (Maybe Int)
resolveLocal name = do
  s <- State.get
  pure $ distance (rScopes s) name

distance :: [Scope] -> Expr.Identifier -> Maybe Int
distance [] _ = Nothing
distance (scope : scopes) name = case Map.lookup (Expr.idName name) scope of
  Just _ -> Just 0
  Nothing -> (+ 1) <$> distance scopes name

declare :: Expr.Identifier -> Resolver ()
declare name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : scopes) -> do
      if Map.member (Expr.idName name) scope
        then reportError name "Already a variable with this name in this scope."
        else State.put s {rScopes = Map.insert (Expr.idName name) False scope : scopes}

define :: Expr.Identifier -> Resolver ()
define name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : scopes) -> State.put s {rScopes = Map.insert (Expr.idName name) True scope : scopes}

beginScope :: Resolver ()
beginScope = do
  State.modify $ \s -> s {rScopes = Map.empty : rScopes s}

endScope :: Resolver ()
endScope = do
  s <- State.get
  case rScopes s of
    [] -> reportError (Expr.Identifier "atEnd" 0) "Unmatched end scope."
    (_ : scopes) -> State.put s {rScopes = scopes}

reportError :: Expr.Identifier -> Text.Text -> Resolver ()
reportError ident message = do
  State.modify $ \s -> s {rErrors = Lox.ResolveError (Expr.idLocation ident) (Expr.idName ident) message : rErrors s}
