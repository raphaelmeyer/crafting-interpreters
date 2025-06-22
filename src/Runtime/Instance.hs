module Runtime.Instance where

import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Parser.Expr as Expr
import qualified Parser.Stmt as Stmt
import qualified Runtime.Types as Runtime

mkInstance :: Runtime.ClassDecl -> IO Runtime.ClassInstance
mkInstance decl = Runtime.ClassInstance decl <$> IORef.newIORef Map.empty

getField :: Text.Text -> Runtime.ClassInstance -> IO (Maybe Runtime.Value)
getField name inst = do
  fields <- IORef.readIORef (Runtime.instFields inst)
  pure $ Map.lookup name fields

setField :: Text.Text -> Runtime.Value -> Runtime.ClassInstance -> IO ()
setField name value inst = do
  IORef.modifyIORef (Runtime.instFields inst) (Map.insert name value)

mkClass :: Text.Text -> [Stmt.Function] -> Runtime.Environment -> IO Runtime.ClassDecl
mkClass clName methods closure = do
  Runtime.ClassDecl clName <$> (IORef.newIORef . Map.fromList . map mkMethods) methods
  where
    mkMethods (Stmt.Function fnName params body) =
      (Expr.idName fnName, Runtime.FunctionDecl (Expr.idName fnName) (map Expr.idName params) body closure)
