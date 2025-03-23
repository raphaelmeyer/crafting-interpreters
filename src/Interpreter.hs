module Interpreter (interpret) where

import qualified Expr
import qualified Lox

interpret :: Expr.Expr -> Lox.Value
interpret = evaluate

evaluate :: Expr.Expr -> Lox.Value
evaluate (Expr.Literal l) = l
evaluate (Expr.Grouping g) = evaluate g
evaluate (Expr.Unary r op) = evalUnary op . evaluate $ r
evaluate _ = undefined

evalUnary :: Expr.UnaryOp -> Lox.Value -> Lox.Value
evalUnary Expr.Neg (Lox.Number n) = Lox.Number (-n)
evalUnary Expr.Not v = Lox.Boolean . not . truthy $ v
evalUnary _ _ = Lox.Nil

truthy :: Lox.Value -> Bool
truthy Lox.Nil = False
truthy (Lox.Boolean b) = b
truthy _ = True
