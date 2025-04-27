{-# LANGUAGE OverloadedStrings #-}

module Interpreter (interpret) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans
import qualified Data.Text as Text
import qualified Error
import qualified Expr
import qualified Literal
import qualified Lox
import qualified Numeric
import qualified Runtime.Environment as Env
import qualified Runtime.Native as Native
import qualified Runtime.Types as Runtime
import qualified Stmt

type Interpreter a = Runtime.Interpreter IO Env.Values a

data Result = Continue | Return Runtime.Value

interpret :: [Stmt.Stmt] -> IO (Lox.Result ())
interpret stmts = do
  result <- State.evalStateT (Except.runExceptT $ execute stmts) Env.make
  case result of
    Left err -> pure $ Left [err]
    Right _ -> pure $ Right ()

execute :: [Stmt.Stmt] -> Interpreter Result
execute [] = pure Continue
execute (stmt : stmts) = do
  result <- statement stmt
  case result of
    Continue -> execute stmts
    Return value -> pure $ Return value

statement :: Stmt.Stmt -> Interpreter Result
statement (Stmt.Expression expr) = do
  Monad.void $ evaluate expr
  pure Continue
statement (Stmt.Print expr) = do
  value <- evaluate expr
  Trans.liftIO $ (putStrLn . toString) value
  pure Continue
statement (Stmt.Variable name expr) = do
  value <- evaluate expr
  Env.define name value
  pure Continue
statement (Stmt.Block block) = do
  executeBlock block
statement (Stmt.If condition thenStmt elseStmt) = do
  isTrue <- truthy <$> evaluate condition
  if isTrue
    then statement thenStmt
    else do
      case elseStmt of
        Just stmt -> statement stmt
        Nothing -> pure Continue
statement stmt@(Stmt.While condition body) = do
  isTrue <- truthy <$> evaluate condition
  if isTrue
    then do
      result <- statement body
      case result of
        Continue -> statement stmt
        Return value -> pure $ Return value
    else pure Continue
statement (Stmt.Function name params body) = do
  Env.define name $ Runtime.Callable (Runtime.Function name params body)
  pure Continue
statement (Stmt.Return expr) = do
  Return <$> evaluate expr

executeBlock :: [Stmt.Stmt] -> Interpreter Result
executeBlock stmts = do
  Env.push
  result <- execute stmts
  Env.pop
  pure result

evaluate :: Expr.Expr -> Interpreter Runtime.Value
evaluate (Expr.Literal l) = pure $ literal l
evaluate (Expr.Grouping g) = evaluate g
evaluate (Expr.Unary op r) = evaluate r >>= evalUnary op
evaluate (Expr.Binary op l r) = do
  left <- evaluate l
  right <- evaluate r
  evalBinary op left right
evaluate (Expr.Variable (Expr.VariableName name loc)) = Env.get (name, loc)
evaluate (Expr.Assign (Expr.VariableName name loc) expr) = do
  value <- evaluate expr
  Env.assign (name, loc) value
  pure value
evaluate (Expr.Logical op l r) = evalLogical op l r
evaluate (Expr.Call calleeExpr args loc) = do
  callee <- evaluate calleeExpr
  argValues <- mapM evaluate args
  invoke callee argValues loc

evalUnary :: Expr.UnaryOp -> Runtime.Value -> Interpreter Runtime.Value
evalUnary (Expr.Operator Expr.Neg _) (Runtime.Number n) = pure $ Runtime.Number (-n)
evalUnary (Expr.Operator Expr.Not _) v = pure . Runtime.Boolean . not . truthy $ v
evalUnary (Expr.Operator Expr.Neg loc) _ = reportError loc "Operand must be a number."

evalBinary :: Expr.BinaryOp -> Runtime.Value -> Runtime.Value -> Interpreter Runtime.Value
evalBinary (Expr.Operator Expr.Equal _) a b = pure $ Runtime.Boolean (a == b)
evalBinary (Expr.Operator Expr.NotEqual _) a b = pure $ Runtime.Boolean (a /= b)
evalBinary (Expr.Operator Expr.Plus _) (Runtime.String a) (Runtime.String b) = pure $ Runtime.String (Text.append a b)
evalBinary (Expr.Operator op _) (Runtime.Number a) (Runtime.Number b) =
  pure $ case op of
    Expr.Plus -> Runtime.Number (a + b)
    Expr.Minus -> Runtime.Number (a - b)
    Expr.Mult -> Runtime.Number (a * b)
    Expr.Div -> Runtime.Number (a / b)
    Expr.Greater -> Runtime.Boolean (a > b)
    Expr.GreaterEqual -> Runtime.Boolean (a >= b)
    Expr.Less -> Runtime.Boolean (a < b)
    Expr.LessEqual -> Runtime.Boolean (a <= b)
evalBinary (Expr.Operator Expr.Plus loc) _ _ = reportError loc "Operands must be two numbers or two strings."
evalBinary (Expr.Operator _ loc) _ _ = reportError loc "Operands must be numbers."

evalLogical :: Expr.LogicalOp -> Expr.Expr -> Expr.Expr -> Interpreter Runtime.Value
evalLogical (Expr.Operator Expr.Or _) l r = do
  a <- evaluate l
  if truthy a then pure a else evaluate r
evalLogical (Expr.Operator Expr.And _) l r = do
  a <- evaluate l
  if truthy a then evaluate r else pure a

literal :: Literal.Value -> Runtime.Value
literal Literal.Nil = Runtime.Nil
literal (Literal.Boolean b) = Runtime.Boolean b
literal (Literal.Number n) = Runtime.Number n
literal (Literal.String s) = Runtime.String s

invoke :: Runtime.Value -> [Runtime.Value] -> Expr.Location -> Interpreter Runtime.Value
invoke (Runtime.Callable declaration) args loc = do
  checkArity declaration args loc
  case declaration of
    Runtime.Clock -> Trans.liftIO Native.clock
    Runtime.Function _ params body -> do
      result <- withGlobals $ do
        bindParameters $ zip params args
        executeBlock body
      case result of
        Continue -> pure Runtime.Nil
        Return value -> pure value
invoke _ _ loc = reportError loc "Can only call functions and classes."

checkArity :: Runtime.Declaration -> [a] -> Expr.Location -> Interpreter ()
checkArity declaration args loc = do
  let arity = Runtime.arity declaration
  Monad.when (arity /= length args) $
    reportError loc $
      Text.concat
        [ "Expected ",
          Text.pack . show $ arity,
          " arguments but got ",
          Text.pack . show . length $ args,
          "."
        ]

bindParameters :: [(Text.Text, Runtime.Value)] -> Interpreter ()
bindParameters = do
  mapM_ (uncurry Env.define)

withGlobals :: Interpreter a -> Interpreter a
withGlobals action = do
  globals <- Env.globals
  original <- State.get
  State.put globals
  result <- action
  State.put original
  pure result

truthy :: Runtime.Value -> Bool
truthy Runtime.Nil = False
truthy (Runtime.Boolean b) = b
truthy _ = True

reportError :: (Monad m) => Expr.Location -> Text.Text -> Except.ExceptT Error.Error m a
reportError loc = Except.throwError . Error.RuntimeError loc

toString :: Runtime.Value -> String
toString (Runtime.Boolean b) = if b then "true" else "false"
toString Runtime.Nil = "nil"
toString (Runtime.Number n) = Numeric.showFFloat Nothing n ""
toString (Runtime.String s) = Text.unpack s
toString (Runtime.Callable Runtime.Clock) = "<native fn>"
toString (Runtime.Callable (Runtime.Function name _ _)) = "<fn " ++ Text.unpack name ++ ">"
