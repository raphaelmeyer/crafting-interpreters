module Interpreter (interpret) where

import qualified Data.Text as Text
import qualified Expr
import qualified Lox

interpret :: Expr.Expr -> Lox.Value
interpret = evaluate

evaluate :: Expr.Expr -> Lox.Value
evaluate (Expr.Literal l) = l
evaluate (Expr.Grouping g) = evaluate g
evaluate (Expr.Unary r op) = evalUnary op . evaluate $ r
evaluate (Expr.Binary l r op) = evalBinary op (evaluate l) (evaluate r)

evalUnary :: Expr.UnaryOp -> Lox.Value -> Lox.Value
evalUnary Expr.Neg (Lox.Number n) = Lox.Number (-n)
evalUnary Expr.Not v = Lox.Boolean . not . truthy $ v
evalUnary _ _ = Lox.Nil

evalBinary :: Expr.BinaryOp -> Lox.Value -> Lox.Value -> Lox.Value
evalBinary Expr.Minus (Lox.Number a) (Lox.Number b) = Lox.Number (a - b)
evalBinary Expr.Mult (Lox.Number a) (Lox.Number b) = Lox.Number (a * b)
evalBinary Expr.Div (Lox.Number a) (Lox.Number b) = Lox.Number (a / b)
evalBinary Expr.Plus (Lox.Number a) (Lox.Number b) = Lox.Number (a + b)
evalBinary Expr.Plus (Lox.String a) (Lox.String b) = Lox.String (Text.append a b)
evalBinary Expr.Greater (Lox.Number a) (Lox.Number b) = Lox.Boolean (a > b)
evalBinary Expr.GreaterEqual (Lox.Number a) (Lox.Number b) = Lox.Boolean (a >= b)
evalBinary Expr.Less (Lox.Number a) (Lox.Number b) = Lox.Boolean (a < b)
evalBinary Expr.LessEqual (Lox.Number a) (Lox.Number b) = Lox.Boolean (a <= b)
evalBinary Expr.Equal a b = Lox.Boolean (a == b)
evalBinary Expr.NotEqual a b = Lox.Boolean (a /= b)
evalBinary _ _ _ = Lox.Nil

truthy :: Lox.Value -> Bool
truthy Lox.Nil = False
truthy (Lox.Boolean b) = b
truthy _ = True
