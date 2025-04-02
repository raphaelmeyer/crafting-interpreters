{-# LANGUAGE OverloadedStrings #-}

module Interpreter (interpret) where

import qualified Data.Text as Text
import qualified Error
import qualified Expr
import qualified Lox
import qualified Stmt

type Result = Lox.Result Lox.Value

interpret :: [Stmt.Stmt] -> IO (Lox.Result ())
interpret (Stmt.Expression expr : stmts) =
  case evaluate expr of
    Left err -> pure $ Left err
    Right _ -> interpret stmts
interpret (Stmt.Print expr : stmts) =
  case evaluate expr of
    Left err -> pure $ Left err
    Right result -> do
      print result
      interpret stmts
interpret [] = pure $ Right ()

evaluate :: Expr.Expr -> Result
evaluate (Expr.Literal l) = Right l
evaluate (Expr.Grouping g) = evaluate g
evaluate (Expr.Unary r op) = evaluate r >>= evalUnary op
evaluate (Expr.Binary l r op) = do
  left <- evaluate l
  right <- evaluate r
  evalBinary op left right

evalUnary :: Expr.UnaryOp -> Lox.Value -> Result
evalUnary Expr.Neg (Lox.Number n) = Right $ Lox.Number (-n)
evalUnary Expr.Not v = Right . Lox.Boolean . not . truthy $ v
evalUnary Expr.Neg _ = Left [Error.Error 0 "Operand must be a number."]

evalBinary :: Expr.BinaryOp -> Lox.Value -> Lox.Value -> Result
evalBinary Expr.Equal a b = Right $ Lox.Boolean (a == b)
evalBinary Expr.NotEqual a b = Right $ Lox.Boolean (a /= b)
evalBinary Expr.Plus (Lox.String a) (Lox.String b) = Right $ Lox.String (Text.append a b)
evalBinary op (Lox.Number a) (Lox.Number b) =
  Right $ case op of
    Expr.Plus -> Lox.Number (a + b)
    Expr.Minus -> Lox.Number (a - b)
    Expr.Mult -> Lox.Number (a * b)
    Expr.Div -> Lox.Number (a / b)
    Expr.Greater -> Lox.Boolean (a > b)
    Expr.GreaterEqual -> Lox.Boolean (a >= b)
    Expr.Less -> Lox.Boolean (a < b)
    Expr.LessEqual -> Lox.Boolean (a <= b)
evalBinary Expr.Plus _ _ = Left [Error.Error 0 "Operands must be two numbers or two strings."]
evalBinary _ _ _ = Left [Error.Error 0 "Operands must be numbers."]

truthy :: Lox.Value -> Bool
truthy Lox.Nil = False
truthy (Lox.Boolean b) = b
truthy _ = True
