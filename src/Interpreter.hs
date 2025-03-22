module Interpreter (interpret) where

import qualified Expr
import qualified Lox

interpret :: Expr.Expr -> Lox.Value
interpret (Expr.Literal l) = l
interpret _ = undefined
