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
import qualified Literal
import qualified Lox
import qualified Native
import qualified Runtime
import qualified Stmt

type Interpreter a = Except.ExceptT Error.Error (Env.Environment IO) a

interpret :: [Stmt.Stmt] -> IO (Lox.Result ())
interpret stmts = do
  result <- State.evalStateT (Except.runExceptT $ execute stmts) Env.make
  case result of
    Left err -> pure $ Left [err]
    Right _ -> pure $ Right ()

execute :: [Stmt.Stmt] -> Interpreter ()
execute = Monad.mapM_ statement

statement :: Stmt.Stmt -> Interpreter ()
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
statement (Stmt.Function name params body) = do
  Trans.lift . Env.define name $ Runtime.Callable (Runtime.Function params body)

executeBlock :: [Stmt.Stmt] -> Interpreter ()
executeBlock stmts = do
  Trans.lift Env.push
  execute stmts
  Env.pop

evaluate :: Expr.Expr -> Interpreter Runtime.Value
evaluate (Expr.Literal l) = pure $ literal l
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

evalUnary :: Expr.UnaryOp -> Runtime.Value -> Interpreter Runtime.Value
evalUnary Expr.Neg (Runtime.Number n) = pure $ Runtime.Number (-n)
evalUnary Expr.Not v = pure . Runtime.Boolean . not . truthy $ v
evalUnary Expr.Neg _ = reportError "Operand must be a number."

evalBinary :: Expr.BinaryOp -> Runtime.Value -> Runtime.Value -> Interpreter Runtime.Value
evalBinary Expr.Equal a b = pure $ Runtime.Boolean (a == b)
evalBinary Expr.NotEqual a b = pure $ Runtime.Boolean (a /= b)
evalBinary Expr.Plus (Runtime.String a) (Runtime.String b) = pure $ Runtime.String (Text.append a b)
evalBinary op (Runtime.Number a) (Runtime.Number b) =
  pure $ case op of
    Expr.Plus -> Runtime.Number (a + b)
    Expr.Minus -> Runtime.Number (a - b)
    Expr.Mult -> Runtime.Number (a * b)
    Expr.Div -> Runtime.Number (a / b)
    Expr.Greater -> Runtime.Boolean (a > b)
    Expr.GreaterEqual -> Runtime.Boolean (a >= b)
    Expr.Less -> Runtime.Boolean (a < b)
    Expr.LessEqual -> Runtime.Boolean (a <= b)
evalBinary Expr.Plus _ _ = reportError "Operands must be two numbers or two strings."
evalBinary _ _ _ = reportError "Operands must be numbers."

evalLogical :: Expr.LogicalOp -> Expr.Expr -> Expr.Expr -> Interpreter Runtime.Value
evalLogical Expr.Or l r = do
  a <- evaluate l
  if truthy a then pure a else evaluate r
evalLogical Expr.And l r = do
  a <- evaluate l
  if truthy a then evaluate r else pure a

literal :: Literal.Value -> Runtime.Value
literal Literal.Nil = Runtime.Nil
literal (Literal.Boolean b) = Runtime.Boolean b
literal (Literal.Number n) = Runtime.Number n
literal (Literal.String s) = Runtime.String s

invoke :: Runtime.Value -> [Runtime.Value] -> Interpreter Runtime.Value
invoke (Runtime.Callable declaration) args = do
  let arity = Runtime.arity declaration
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
    Runtime.Clock -> Trans.liftIO Native.clock
    Runtime.Function _ body -> do
      withGlobals $ executeBlock body
      pure Runtime.Nil
invoke _ _ = reportError "Can only call functions and classes."

withGlobals :: Interpreter a -> Interpreter a
withGlobals action = do
  globals <- Trans.lift Env.globals
  original <- State.get
  State.put globals
  result <- action
  State.put original
  pure result

truthy :: Runtime.Value -> Bool
truthy Runtime.Nil = False
truthy (Runtime.Boolean b) = b
truthy _ = True

reportError :: (Monad m) => Text.Text -> Except.ExceptT Error.Error m a
reportError = Except.throwError . Error.RuntimeError
