{-# LANGUAGE OverloadedStrings #-}

module Resolver.Resolver (resolve) where

import qualified Control.Monad as Monad
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lox
import qualified Parser.Expr as Expr
import qualified Parser.Stmt as Stmt

type Scope = Map.Map Text.Text Bool

data FunctionType = NotFunction | Function | Initializer | Method deriving (Eq)

data ClassType = NotClass | Class deriving (Eq)

data ResolverState = ResolverState
  { rScopes :: [Scope],
    rErrors :: [Lox.Error],
    rFunction :: FunctionType,
    rClass :: ClassType
  }

type Resolver a = State.State ResolverState a

resolve :: [Stmt.Stmt] -> Lox.Result [Stmt.Stmt]
resolve stmts = case rErrors s of
  [] -> Right resolved
  errors -> Left errors
  where
    (resolved, s) = State.runState (program stmts) (ResolverState [] [] NotFunction NotClass)

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
statement (Stmt.Fun function) = do
  declare (Stmt.funName function)
  define (Stmt.funName function)
  Stmt.Fun <$> resolveFunction Function function
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
statement (Stmt.Return loc value) = do
  fun <- rFunction <$> State.get
  Monad.when (fun == NotFunction) $ reportError "return" loc "Can't return from top-level code."
  case value of
    Just expr -> do
      Monad.when (fun == Initializer) $ reportError "return" loc "Can't return a value from an initializer."
      Stmt.Return loc . Just <$> expression expr
    Nothing -> pure $ Stmt.Return loc Nothing
statement (Stmt.While condition body) = do
  resCond <- expression condition
  resBody <- statement body
  pure $ Stmt.While resCond resBody
statement Stmt.Break = pure Stmt.Break
statement (Stmt.Class name superclass methods) = resolveClass name superclass methods

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
expression (Expr.Expr (Expr.Get object name) loc) = do
  resolved <- expression object
  pure $ Expr.Expr (Expr.Get resolved name) loc
expression (Expr.Expr (Expr.Set object name value) loc) = do
  resValue <- expression value
  resObject <- expression object
  pure $ Expr.Expr (Expr.Set resObject name resValue) loc
expression (Expr.Expr (Expr.This _) loc) = do
  cl <- rClass <$> State.get
  Monad.when (cl == NotClass) $ reportError Lox.this loc "Can't use 'this' outside of a class."
  d <- resolveLocal (Expr.Identifier Lox.this loc)
  pure $ Expr.Expr (Expr.This d) loc

resolveFunction :: FunctionType -> Stmt.Function -> Resolver Stmt.Function
resolveFunction functionType (Stmt.Function name params body) = do
  enclosing <- rFunction <$> State.get
  State.modify $ \s -> s {rFunction = functionType}
  beginScope
  mapM_ (\p -> declare p >> define p) params
  resolved <- program body
  endScope
  State.modify $ \s -> s {rFunction = enclosing}
  pure $ Stmt.Function name params resolved

resolveMethod :: Stmt.Function -> Resolver Stmt.Function
resolveMethod method = resolveFunction methodType method
  where
    methodType =
      if Expr.idName (Stmt.funName method) == Lox.initializer
        then Initializer
        else Method

resolveClass :: Expr.Identifier -> Maybe Expr.Superclass -> [Stmt.Function] -> Resolver Stmt.Stmt
resolveClass name superclass methods = do
  enclosing <- rClass <$> State.get
  State.modify $ \s -> s {rClass = Class}
  declare name
  define name
  beginClassScope
  resolved <- mapM resolveMethod methods
  endScope
  State.modify $ \s -> s {rClass = enclosing}
  pure (Stmt.Class name superclass resolved)

checkNoSelfReference :: Expr.Identifier -> Resolver ()
checkNoSelfReference name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : _) -> case Map.lookup (Expr.idName name) scope of
      Just False -> reportError (Expr.idName name) (Expr.idLocation name) "Can't read local variable in its own initializer."
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
        then reportError (Expr.idName name) (Expr.idLocation name) "Already a variable with this name in this scope."
        else State.put s {rScopes = Map.insert (Expr.idName name) False scope : scopes}

define :: Expr.Identifier -> Resolver ()
define name = do
  s <- State.get
  case rScopes s of
    [] -> pure ()
    (scope : scopes) -> State.put s {rScopes = Map.insert (Expr.idName name) True scope : scopes}

beginClassScope :: Resolver ()
beginClassScope = State.modify $ \s -> s {rScopes = Map.singleton Lox.this True : rScopes s}

beginScope :: Resolver ()
beginScope = do
  State.modify $ \s -> s {rScopes = Map.empty : rScopes s}

endScope :: Resolver ()
endScope = do
  s <- State.get
  case rScopes s of
    [] -> reportError "atEnd" 0 "Unmatched end scope."
    (_ : scopes) -> State.put s {rScopes = scopes}

reportError :: Text.Text -> Expr.Location -> Text.Text -> Resolver ()
reportError name loc message = do
  State.modify $ \s -> s {rErrors = Lox.ResolveError loc name message : rErrors s}
