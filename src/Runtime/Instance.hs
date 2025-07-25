module Runtime.Instance (findMethod, getField, getMethod, mkClass, mkInstance, setField) where

import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lox
import qualified Parser.Expr as Expr
import qualified Parser.Stmt as Stmt
import qualified Runtime.Types as Runtime

mkInstance :: Runtime.ClassDecl -> IO Runtime.ClassInstance
mkInstance decl = Runtime.ClassInstance decl <$> IORef.newIORef Map.empty

mkClass :: Text.Text -> Maybe Runtime.ClassDecl -> [Stmt.Function] -> Runtime.Environment -> IO Runtime.ClassDecl
mkClass clName superclass methods closure = do
  let clMethods = Map.fromList . map (tuple . mkMethod closure) $ methods
  let arity = case Map.lookup Lox.initializer clMethods of
        Just (Runtime.FunctionDecl {Runtime.funParameters = params}) -> Just $ length params
        Nothing -> Nothing
  methodsRef <- IORef.newIORef clMethods
  pure $ Runtime.ClassDecl clName superclass methodsRef arity
  where
    tuple function = (Runtime.funName function, function)

mkMethod :: Runtime.Environment -> Stmt.Function -> Runtime.FunctionDecl
mkMethod closure (Stmt.Function name params body) =
  Runtime.FunctionDecl fnName (map Expr.idName params) body closure (fnName == Lox.initializer)
  where
    fnName = Expr.idName name

getField :: Text.Text -> Runtime.ClassInstance -> IO (Maybe Runtime.Value)
getField name inst = do
  fields <- IORef.readIORef (Runtime.instFields inst)
  pure $ Map.lookup name fields

setField :: Text.Text -> Runtime.Value -> Runtime.ClassInstance -> IO ()
setField name value inst = do
  IORef.modifyIORef (Runtime.instFields inst) (Map.insert name value)

getMethod :: Text.Text -> Runtime.ClassInstance -> IO (Maybe Runtime.Value)
getMethod name inst = do
  method <- findMethod name (Runtime.instClass inst)
  pure $ Runtime.Callable . Runtime.Function <$> method

findMethod :: Text.Text -> Runtime.ClassDecl -> IO (Maybe Runtime.FunctionDecl)
findMethod name decl = do
  methods <- IORef.readIORef (Runtime.clMethods decl)
  case Map.lookup name methods of
    Just method -> pure $ Just method
    Nothing -> case Runtime.clSuper decl of
      Just superclass -> findMethod name superclass
      Nothing -> pure Nothing
