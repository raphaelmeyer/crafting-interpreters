{-# LANGUAGE OverloadedStrings #-}

module Runtime.Interpreter (interpret, makeEnvironment, Runtime.Environment) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans
import qualified Data.Text as Text
import qualified Lox
import qualified Numeric
import qualified Parser.Expr as Expr
import qualified Parser.Literal as Literal
import qualified Parser.Stmt as Stmt
import qualified Runtime.Environment as Env
import qualified Runtime.Instance as Instance
import qualified Runtime.Native as Native
import qualified Runtime.Types as Runtime

type Interpreter a = Runtime.Interpreter IO a

data Result = Continue | Return Runtime.Value | Break

interpret :: [Stmt.Stmt] -> Runtime.Environment -> IO (Lox.Result ())
interpret stmts globals = do
  result <- State.evalStateT (Except.runExceptT $ execute stmts) globals
  case result of
    Left err -> pure $ Left [err]
    Right Break -> pure $ Left [Lox.RuntimeError 0 "Cannot break out of nothing."]
    Right _ -> pure $ Right ()

makeEnvironment :: IO Runtime.Environment
makeEnvironment = Env.make

execute :: [Stmt.Stmt] -> Interpreter Result
execute [] = pure Continue
execute (stmt : stmts) = do
  result <- statement stmt
  case result of
    Continue -> execute stmts
    Return value -> pure $ Return value
    Break -> pure Break

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
  Env.define (Expr.idName name) value
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
        Break -> pure Continue
    else pure Continue
statement (Stmt.Fun (Stmt.Function name params body)) = do
  closure <- Env.current
  let paramNames = map Expr.idName params
  Env.define (Expr.idName name) $ Runtime.Callable . Runtime.Function $ Runtime.FunctionDecl (Expr.idName name) paramNames body closure False
  pure Continue
statement (Stmt.Return _ value) = case value of
  Just expr -> Return <$> evaluate expr
  Nothing -> pure $ Return Runtime.Nil
statement Stmt.Break = pure Break
statement (Stmt.Class name superName methods) = do
  superclass <- mapM extend superName
  closure <- Env.current
  Env.define (Expr.idName name) Runtime.Nil
  cl <- Trans.liftIO $ Instance.mkClass (Expr.idName name) superclass methods closure
  Env.assign (Expr.idName name) (Expr.idLocation name) $ Runtime.Callable . Runtime.Class $ cl
  pure Continue

executeBlock :: [Stmt.Stmt] -> Interpreter Result
executeBlock stmts = do
  Env.push
  result <- execute stmts
  Env.pop
  pure result

extend :: Expr.Superclass -> Interpreter Runtime.ClassDecl
extend (Expr.Superclass name depth) = do
  superclass <- Env.getAt (Expr.idName name) depth (Expr.idLocation name)
  case superclass of
    Runtime.Callable (Runtime.Class decl) -> pure decl
    _ -> reportError (Expr.idLocation name) "Superclass must be a class."

evaluate :: Expr.Expr -> Interpreter Runtime.Value
evaluate (Expr.Expr (Expr.Literal l) _) = pure $ literal l
evaluate (Expr.Expr (Expr.Grouping g) _) = evaluate g
evaluate (Expr.Expr (Expr.Unary op r) loc) = do
  right <- evaluate r
  evalUnary op right loc
evaluate (Expr.Expr (Expr.Binary op l r) loc) = do
  left <- evaluate l
  right <- evaluate r
  evalBinary op left right loc
evaluate (Expr.Expr (Expr.Variable name depth) _) =
  Env.getAt (Expr.idName name) depth (Expr.idLocation name)
evaluate (Expr.Expr (Expr.Assign name expr depth) _) = do
  value <- evaluate expr
  Env.assignAt (Expr.idName name) depth (Expr.idLocation name) value
  pure value
evaluate (Expr.Expr (Expr.Logical op l r) _) = evalLogical op l r
evaluate (Expr.Expr (Expr.Call calleeExpr args) loc) = do
  callee <- evaluate calleeExpr
  argValues <- mapM evaluate args
  invoke callee argValues loc
evaluate (Expr.Expr (Expr.Get objectExpr name) loc) = do
  object <- evaluate objectExpr
  case object of
    Runtime.Instance clInst -> getField name clInst
    _ -> reportError loc "Only instances have properties."
evaluate (Expr.Expr (Expr.Set objectExpr name valueExpr) loc) = do
  object <- evaluate objectExpr
  case object of
    Runtime.Instance clInst -> do
      value <- evaluate valueExpr
      setField name value clInst
      pure value
    _ -> reportError loc "Only instances have fields."
evaluate (Expr.Expr (Expr.This depth) loc) = Env.getAt Lox.this depth loc

evalUnary :: Expr.UnaryOp -> Runtime.Value -> Expr.Location -> Interpreter Runtime.Value
evalUnary Expr.Neg (Runtime.Number n) _ = pure $ Runtime.Number (-n)
evalUnary Expr.Not v _ = pure . Runtime.Boolean . not . truthy $ v
evalUnary Expr.Neg _ loc = reportError loc "Operand must be a number."

evalBinary :: Expr.BinaryOp -> Runtime.Value -> Runtime.Value -> Expr.Location -> Interpreter Runtime.Value
evalBinary Expr.Equal a b _ = pure $ Runtime.Boolean (a == b)
evalBinary Expr.NotEqual a b _ = pure $ Runtime.Boolean (a /= b)
evalBinary Expr.Plus (Runtime.String a) (Runtime.String b) _ = pure $ Runtime.String (Text.append a b)
evalBinary op (Runtime.Number a) (Runtime.Number b) _ =
  pure $ case op of
    Expr.Plus -> Runtime.Number (a + b)
    Expr.Minus -> Runtime.Number (a - b)
    Expr.Mult -> Runtime.Number (a * b)
    Expr.Div -> Runtime.Number (a / b)
    Expr.Greater -> Runtime.Boolean (a > b)
    Expr.GreaterEqual -> Runtime.Boolean (a >= b)
    Expr.Less -> Runtime.Boolean (a < b)
    Expr.LessEqual -> Runtime.Boolean (a <= b)
evalBinary Expr.Plus _ _ loc = reportError loc "Operands must be two numbers or two strings."
evalBinary _ _ _ loc = reportError loc "Operands must be numbers."

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

truthy :: Runtime.Value -> Bool
truthy Runtime.Nil = False
truthy (Runtime.Boolean b) = b
truthy _ = True

invoke :: Runtime.Value -> [Runtime.Value] -> Expr.Location -> Interpreter Runtime.Value
invoke (Runtime.Callable declaration) args loc = do
  checkArity declaration args loc
  case declaration of
    Runtime.Clock -> Trans.liftIO Native.clock
    Runtime.Function (Runtime.FunctionDecl _ params body closure isInitializer) -> do
      result <- withEnvironment closure $ do
        Env.push
        bindParameters $ zip params args
        execute body >>= returnThisIf isInitializer loc
      case result of
        Continue -> pure Runtime.Nil
        Return value -> pure value
        Break -> reportError loc "Can not break out of a function."
    Runtime.Class decl -> do
      inst <- Trans.liftIO $ Instance.mkInstance decl
      maybeInit <- findMethod Lox.initializer inst
      case maybeInit of
        Just initializer -> do
          bound <- bind inst initializer
          Monad.void $ invoke bound args loc
        Nothing -> pure ()
      pure $ Runtime.Instance inst
invoke _ _ loc = reportError loc "Can only call functions and classes."

checkArity :: Runtime.Declaration -> [a] -> Expr.Location -> Interpreter ()
checkArity declaration args loc = do
  let arity = getArity declaration
  Monad.when (arity /= length args) $
    reportError loc $
      Text.concat
        [ "Expected ",
          Text.pack . show $ arity,
          " arguments but got ",
          Text.pack . show . length $ args,
          "."
        ]

getArity :: Runtime.Declaration -> Int
getArity Runtime.Clock = 0
getArity (Runtime.Class (Runtime.ClassDecl {Runtime.clArity = arity})) = arity
getArity (Runtime.Function Runtime.FunctionDecl {Runtime.funParameters = params}) = length params

withEnvironment :: Runtime.Environment -> Interpreter a -> Interpreter a
withEnvironment env action = do
  original <- State.get
  State.put env
  result <- action
  State.put original
  pure result

bindParameters :: [(Text.Text, Runtime.Value)] -> Interpreter ()
bindParameters = do
  mapM_ (uncurry Env.define)

bind :: Runtime.ClassInstance -> Runtime.Value -> Interpreter Runtime.Value
bind inst (Runtime.Callable (Runtime.Function method)) =
  withEnvironment (Runtime.funClosure method) $ do
    Env.push
    Env.define Lox.this $ Runtime.Instance inst
    closure <- Env.current
    pure $ Runtime.Callable (Runtime.Function method {Runtime.funClosure = closure})
bind _ _ = reportError 0 "Can only bind callables."

getField :: Expr.Identifier -> Runtime.ClassInstance -> Interpreter Runtime.Value
getField name inst = do
  maybeField <- Trans.liftIO $ Instance.getField (Expr.idName name) inst
  case maybeField of
    Just value -> pure value
    Nothing -> do
      maybeMethod <- findMethod (Expr.idName name) inst
      case maybeMethod of
        Just method -> bind inst method
        Nothing -> reportError (Expr.idLocation name) $ Text.concat ["Undefined property '", Expr.idName name, "'."]

setField :: Expr.Identifier -> Runtime.Value -> Runtime.ClassInstance -> Interpreter ()
setField name value inst = Trans.liftIO $ Instance.setField (Expr.idName name) value inst

findMethod :: Text.Text -> Runtime.ClassInstance -> Interpreter (Maybe Runtime.Value)
findMethod name inst = Trans.liftIO $ Instance.getMethod name inst

returnThisIf :: Bool -> Expr.Location -> Result -> Interpreter Result
returnThisIf isInitializer loc result =
  if isInitializer
    then Return <$> Env.getAt Lox.this (Just 1) loc
    else pure result

reportError :: (Monad m) => Expr.Location -> Text.Text -> Except.ExceptT Lox.Error m a
reportError loc = Except.throwError . Lox.RuntimeError loc

toString :: Runtime.Value -> String
toString (Runtime.Boolean b) = if b then "true" else "false"
toString Runtime.Nil = "nil"
toString (Runtime.Number n) = Numeric.showFFloat Nothing n ""
toString (Runtime.String s) = Text.unpack s
toString (Runtime.Callable Runtime.Clock) = "<native fn>"
toString (Runtime.Callable (Runtime.Function (Runtime.FunctionDecl name _ _ _ _))) = "<fn " ++ Text.unpack name ++ ">"
toString (Runtime.Callable (Runtime.Class (Runtime.ClassDecl {Runtime.clName = name}))) = Text.unpack name
toString (Runtime.Instance (Runtime.ClassInstance (Runtime.ClassDecl {Runtime.clName = name}) _)) = Text.unpack name ++ " instance"
