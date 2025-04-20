{-# LANGUAGE OverloadedStrings #-}

module Interpreter (interpret) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans
import qualified Data.Text as Text
import qualified Environment as Env
import qualified Error
import qualified Expr
import qualified Lox
import qualified Native
import qualified Stmt

type Interpreter' a = Except.ExceptT Error.Error (Env.Environment IO) a

type Interpreter = Interpreter' Lox.Value

interpret :: [Stmt.Stmt] -> IO (Lox.Result ())
interpret stmts = do
  result <- State.evalStateT (Except.runExceptT $ execute stmts) Env.make
  case result of
    Left err -> pure $ Left [err]
    Right _ -> pure $ Right ()

execute :: [Stmt.Stmt] -> Interpreter' ()
execute = Monad.mapM_ statement

statement :: Stmt.Stmt -> Interpreter' ()
statement (Stmt.Expression expr) = do
  Monad.void $ evaluate expr
statement (Stmt.Print expr) = do
  value <- evaluate expr
  Trans.liftIO $ print value
statement (Stmt.Variable name expr) = do
  value <- evaluate expr
  Trans.lift $ Env.define name value
statement (Stmt.Block block) = do
  executeBlock block
statement (Stmt.If condition thenStmt elseStmt) = do
  isTrue <- truthy <$> evaluate condition
  if isTrue
    then statement thenStmt
    else Monad.mapM_ statement elseStmt
statement stmt@(Stmt.While condition body) = do
  isTrue <- truthy <$> evaluate condition
  Monad.when isTrue $ do
    statement body
    statement stmt

executeBlock :: [Stmt.Stmt] -> Interpreter' ()
executeBlock stmts = do
  Trans.lift Env.push
  execute stmts
  Env.pop

evaluate :: Expr.Expr -> Interpreter
evaluate (Expr.Literal l) = pure l
evaluate (Expr.Grouping g) = evaluate g
evaluate (Expr.Unary op r) = evaluate r >>= evalUnary op
evaluate (Expr.Binary op l r) = do
  left <- evaluate l
  right <- evaluate r
  evalBinary op left right
evaluate (Expr.Variable name) = Env.get name
evaluate (Expr.Assign name expr) = do
  value <- evaluate expr
  Env.assign name value
  pure value
evaluate (Expr.Logical op l r) = evalLogical op l r
evaluate (Expr.Call calleeExpr args) = do
  callee <- evaluate calleeExpr
  argValues <- mapM evaluate args
  invoke callee argValues

evalUnary :: Expr.UnaryOp -> Lox.Value -> Interpreter
evalUnary Expr.Neg (Lox.Number n) = pure $ Lox.Number (-n)
evalUnary Expr.Not v = pure . Lox.Boolean . not . truthy $ v
evalUnary Expr.Neg _ = reportError "Operand must be a number."

evalBinary :: Expr.BinaryOp -> Lox.Value -> Lox.Value -> Interpreter
evalBinary Expr.Equal a b = pure $ Lox.Boolean (a == b)
evalBinary Expr.NotEqual a b = pure $ Lox.Boolean (a /= b)
evalBinary Expr.Plus (Lox.String a) (Lox.String b) = pure $ Lox.String (Text.append a b)
evalBinary op (Lox.Number a) (Lox.Number b) =
  pure $ case op of
    Expr.Plus -> Lox.Number (a + b)
    Expr.Minus -> Lox.Number (a - b)
    Expr.Mult -> Lox.Number (a * b)
    Expr.Div -> Lox.Number (a / b)
    Expr.Greater -> Lox.Boolean (a > b)
    Expr.GreaterEqual -> Lox.Boolean (a >= b)
    Expr.Less -> Lox.Boolean (a < b)
    Expr.LessEqual -> Lox.Boolean (a <= b)
evalBinary Expr.Plus _ _ = reportError "Operands must be two numbers or two strings."
evalBinary _ _ _ = reportError "Operands must be numbers."

evalLogical :: Expr.LogicalOp -> Expr.Expr -> Expr.Expr -> Interpreter
evalLogical Expr.Or l r = do
  a <- evaluate l
  if truthy a then pure a else evaluate r
evalLogical Expr.And l r = do
  a <- evaluate l
  if truthy a then evaluate r else pure a

invoke :: Lox.Value -> [Lox.Value] -> Interpreter
invoke (Lox.Callable declaration) args = do
  let arity = Lox.arity declaration
  Monad.when (arity /= length args) $
    reportError $
      Text.concat
        [ "Expected ",
          Text.pack . show $ arity,
          " arguments but got ",
          Text.pack . show . length $ args,
          "."
        ]
  case declaration of
    Lox.Clock -> Trans.liftIO Native.clock
invoke _ _ = reportError "Can only call functions and classes."

truthy :: Lox.Value -> Bool
truthy Lox.Nil = False
truthy (Lox.Boolean b) = b
truthy _ = True

reportError :: (Monad m) => Text.Text -> Except.ExceptT Error.Error m a
reportError = Except.throwError . Error.RuntimeError
