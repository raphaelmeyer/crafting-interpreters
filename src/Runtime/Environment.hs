{-# LANGUAGE OverloadedStrings #-}

module Runtime.Environment (assign, current, define, get, globals, make, pop, push) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Error
import qualified Runtime.Types as Runtime

type Interpreter a = Runtime.Interpreter IO a

make :: IO Runtime.Environment
make = do
  m <- IORef.newIORef $ Map.insert "clock" (Runtime.Callable Runtime.Clock) Map.empty
  pure $ Runtime.Global m

globals :: Interpreter Runtime.Environment
globals = globals' <$> State.get

current :: Interpreter Runtime.Environment
current = State.get

get :: (Text.Text, Int) -> Interpreter Runtime.Value
get (name, loc) = do
  env <- State.get
  maybeValue <- Trans.liftIO $ get' name env
  case maybeValue of
    Just value -> pure value
    Nothing -> reportError loc $ Text.concat ["Undefined variable '", name, "'."]

define :: Text.Text -> Runtime.Value -> Interpreter ()
define name value = do
  env <- State.get
  Trans.liftIO $ case env of
    Runtime.Global scope -> insert name value scope
    Runtime.Local scope _ -> insert name value scope

assign :: (Text.Text, Int) -> Runtime.Value -> Interpreter ()
assign (name, loc) value = do
  env <- State.get
  assigned <- Trans.liftIO $ assign' name value env
  if assigned
    then pure ()
    else reportError loc $ Text.concat ["Undefined variable '", name, "'."]

push :: Interpreter ()
push = do
  env <- State.get
  scope <- Trans.liftIO $ IORef.newIORef Map.empty
  State.put $ Runtime.Local scope env

pop :: Interpreter ()
pop = do
  env <- State.get
  case env of
    Runtime.Local _ parent -> State.put parent
    Runtime.Global _ -> reportError 0 "Can not pop global environment."

globals' :: Runtime.Environment -> Runtime.Environment
globals' env = do
  case env of
    Runtime.Global scope -> Runtime.Global scope
    Runtime.Local _ parent -> globals' parent

get' :: Text.Text -> Runtime.Environment -> IO (Maybe Runtime.Value)
get' name env = case env of
  Runtime.Global scope -> Runtime.Environment.lookup name scope
  Runtime.Local scope parent -> do
    maybeValue <- Runtime.Environment.lookup name scope
    case maybeValue of
      Just value -> pure $ Just value
      Nothing -> get' name parent

assign' :: Text.Text -> Runtime.Value -> Runtime.Environment -> IO Bool
assign' name value (Runtime.Global scope) = do
  maybeValue <- Runtime.Environment.lookup name scope
  case maybeValue of
    Just _ -> do
      insert name value scope
      pure True
    Nothing -> pure False
assign' name value (Runtime.Local scope parent) = do
  maybeValue <- Runtime.Environment.lookup name scope
  case maybeValue of
    Just _ -> do
      insert name value scope
      pure True
    Nothing -> assign' name value parent

insert :: Text.Text -> Runtime.Value -> Runtime.Scope -> IO ()
insert name value scope = IORef.modifyIORef scope (Map.insert name value)

lookup :: Text.Text -> Runtime.Scope -> IO (Maybe Runtime.Value)
lookup name scope = do
  storage <- IORef.readIORef scope
  pure $ Map.lookup name storage

reportError :: Int -> Text.Text -> Interpreter a
reportError loc = Except.throwError . Error.RuntimeError loc
