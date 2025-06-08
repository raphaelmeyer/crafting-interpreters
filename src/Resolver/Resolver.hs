{-# LANGUAGE OverloadedStrings #-}

module Resolver.Resolver (resolve) where

import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lox
import qualified Parser.Expr as Expr
import qualified Parser.Stmt as Stmt

type Scope = Map.Map Expr.Identifier Bool

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
statement stmt = pure stmt

expression :: Expr.Expr -> Resolver Expr.Expr
expression (Expr.Expr (Expr.Variable name _) loc) = do
  checkNoSelfReference loc name
  d <- resolveLocal name
  pure (Expr.Expr (Expr.Variable name d) loc)
expression (Expr.Expr (Expr.Assign name expr _) loc) = do
  resolved <- expression expr
  d <- resolveLocal name
  pure $ Expr.Expr (Expr.Assign name resolved d) loc
expression expr = pure expr

resolveFunction :: Expr.Identifier -> [Expr.Identifier] -> [Stmt.Stmt] -> Resolver Stmt.Stmt
resolveFunction name params body = do
  beginScope
  mapM_ (\p -> declare p >> define p) params
  resolved <- program body
  endScope
  pure $ Stmt.Function name params resolved

checkNoSelfReference :: Expr.Location -> Text.Text -> Resolver ()
checkNoSelfReference loc name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : _) -> case Map.lookup name scope of
      Just False -> reportError loc name "Can't read local variable in its own initializer."
      _ -> pure ()

resolveLocal :: Expr.Identifier -> Resolver (Maybe Int)
resolveLocal name = do
  s <- State.get
  pure $ distance (rScopes s) name

distance :: [Scope] -> Expr.Identifier -> Maybe Int
distance [] _ = Nothing
distance (scope : scopes) name = case Map.lookup name scope of
  Just _ -> Just 0
  Nothing -> (+ 1) <$> distance scopes name

declare :: Expr.Identifier -> Resolver ()
declare name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : scopes) -> State.put s {rScopes = Map.insert name False scope : scopes}

define :: Expr.Identifier -> Resolver ()
define name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : scopes) -> State.put s {rScopes = Map.insert name True scope : scopes}

beginScope :: Resolver ()
beginScope = do
  State.modify $ \s -> s {rScopes = Map.empty : rScopes s}

endScope :: Resolver ()
endScope = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (_ : scopes) -> State.put s {rScopes = scopes}

reportError :: Expr.Location -> Text.Text -> Text.Text -> Resolver ()
reportError loc lexeme message = do
  State.modify $ \s -> s {rErrors = Lox.ResolveError loc lexeme message : rErrors s}
