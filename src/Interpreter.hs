module Interpreter (interpret) where

import qualified Data.Text as Text
import qualified Expr

data Value
  = Number Double
  | String Text.Text
  | Boolean Bool
  | Nil
  deriving (Eq, Show)

interpret :: Expr.Expr -> Value
interpret (Expr.Literal l) = case l of
  Expr.Number n -> Number n
  Expr.String s -> String s
  Expr.Boolean b -> Boolean b
  Expr.Nil -> Nil
interpret _ = undefined
