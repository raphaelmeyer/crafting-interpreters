module Runtime.Instance where

import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
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
