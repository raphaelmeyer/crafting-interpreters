module Interpreter (interpret) where

import qualified Expr
import qualified Lox

interpret :: Expr.Expr -> Lox.Value
interpret = evaluate

evaluate :: Expr.Expr -> Lox.Value
evaluate (Expr.Literal l) = l
evaluate (Expr.Grouping g) = evaluate g
evaluate _ = undefined
