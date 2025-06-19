module Runtime.Instance where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Runtime.Types as Runtime

mkInstance :: Runtime.ClassDecl -> Runtime.ClassInstance
mkInstance decl = Runtime.ClassInstance decl Map.empty

getField :: Text.Text -> Runtime.ClassInstance -> Maybe Runtime.Value
getField name inst = Map.lookup name (Runtime.instFields inst)
