{-# LANGUAGE OverloadedStrings #-}

module Interpreter (interpret) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as Text
import qualified Environment as Env
import qualified Error
import qualified Expr
import qualified Lox
import qualified Stmt

type Interpreter m = Except.ExceptT Error.Error (Env.Environment m) Lox.Value

interpret :: [Stmt.Stmt] -> IO (Lox.Result ())
interpret stmts = do
  result <- State.evalStateT (Except.runExceptT $ statement stmts) Env.empty
  case result of
    Left err -> pure $ Left [err]
    Right _ -> pure $ Right ()

statement :: [Stmt.Stmt] -> Except.ExceptT Error.Error (Env.Environment IO) ()
statement (Stmt.Expression expr : stmts) = do
  _ <- evaluate expr
  statement stmts
statement (Stmt.Print expr : stmts) = do
  value <- evaluate expr
  Except.liftIO $ print value
  statement stmts
statement (Stmt.Variable name expr : stmts) = do
  value <- evaluate expr
  Except.lift $ Env.define name value
  statement stmts
statement (Stmt.Block block : stmts) = do
  executeBlock block
  statement stmts
statement [] = pure ()

executeBlock :: [Stmt.Stmt] -> Except.ExceptT Error.Error (Env.Environment IO) ()
executeBlock stmts = do
  Except.lift $ Env.push
  statement stmts
  Env.pop

evaluate :: (Monad m) => Expr.Expr -> Interpreter m
evaluate (Expr.Literal l) = pure l
evaluate (Expr.Grouping g) = evaluate g
evaluate (Expr.Unary r op) = evaluate r >>= evalUnary op
evaluate (Expr.Binary l r op) = do
  left <- evaluate l
  right <- evaluate r
  evalBinary op left right
evaluate (Expr.Variable name) = Env.get name
evaluate (Expr.Assign name expr) = do
  value <- evaluate expr
  Env.assign name value
  pure value

evalUnary :: (Monad m) => Expr.UnaryOp -> Lox.Value -> Interpreter m
evalUnary Expr.Neg (Lox.Number n) = pure $ Lox.Number (-n)
evalUnary Expr.Not v = pure . Lox.Boolean . not . truthy $ v
evalUnary Expr.Neg _ = Except.throwError $ Error.Error 0 "Operand must be a number."

evalBinary :: (Monad m) => Expr.BinaryOp -> Lox.Value -> Lox.Value -> Interpreter m
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
evalBinary Expr.Plus _ _ = Except.throwError $ Error.Error 0 "Operands must be two numbers or two strings."
evalBinary _ _ _ = Except.throwError $ Error.Error 0 "Operands must be numbers."

truthy :: Lox.Value -> Bool
truthy Lox.Nil = False
truthy (Lox.Boolean b) = b
truthy _ = True
