{-# LANGUAGE OverloadedStrings #-}

module Runtime.Environment (assign, assignAt, current, define, make, pop, push, getAt) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lox
import qualified Runtime.Types as Runtime

type Interpreter a = Runtime.Interpreter IO a

make :: IO Runtime.Environment
make = do
  m <- IORef.newIORef $ Map.insert "clock" (Runtime.Callable Runtime.Clock) Map.empty
  pure $ Runtime.Global m

current :: Interpreter Runtime.Environment
current = State.get

getAt :: Text.Text -> Maybe Int -> Int -> Interpreter Runtime.Value
getAt name depth loc = do
  env <- State.get
  maybeValue <- case ancestor env depth of
    Just scope -> Trans.liftIO $ Runtime.Environment.lookup name scope
    Nothing -> reportError loc $ Text.concat ["Unresolved variable '", name, "'."]
  case maybeValue of
    Just value -> pure value
    Nothing -> reportError loc $ Text.concat ["Undefined variable '", name, "'."]

define :: Text.Text -> Runtime.Value -> Interpreter ()
define name value = do
  env <- State.get
  Trans.liftIO $ case env of
    Runtime.Global scope -> insert name value scope
    Runtime.Local scope _ -> insert name value scope

assignAt :: Text.Text -> Maybe Int -> Int -> Runtime.Value -> Interpreter ()
assignAt name depth loc value = do
  env <- State.get
  case ancestor env depth of
    Just scope -> do
      exists <- Trans.liftIO $ Runtime.Environment.lookup name scope
      case exists of
        Just _ -> Trans.liftIO $ Runtime.Environment.insert name value scope
        Nothing -> reportError loc $ Text.concat ["Undefined variable '", name, "'."]
    Nothing -> reportError loc $ Text.concat ["Unresolved variable '", name, "'."]

assign :: Text.Text -> Int -> Runtime.Value -> Interpreter ()
assign name loc value = do
  env <- State.get
  maybeScope <- Trans.liftIO $ search name env
  case maybeScope of
    Just scope -> Trans.liftIO $ Runtime.Environment.insert name value scope
    Nothing -> reportError loc $ Text.concat ["Undefined variable '", name, "'."]

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

insert :: Text.Text -> Runtime.Value -> Runtime.Scope -> IO ()
insert name value scope = IORef.modifyIORef scope (Map.insert name value)

lookup :: Text.Text -> Runtime.Scope -> IO (Maybe Runtime.Value)
lookup name scope = do
  storage <- IORef.readIORef scope
  pure $ Map.lookup name storage

ancestor :: Runtime.Environment -> Maybe Int -> Maybe Runtime.Scope
ancestor env depth = case depth of
  Just d -> local env d
  Nothing -> Just $ global env

local :: Runtime.Environment -> Int -> Maybe Runtime.Scope
local (Runtime.Global _) _ = Nothing
local (Runtime.Local scope _) 0 = Just scope
local (Runtime.Local _ parent) depth = local parent (depth - 1)

global :: Runtime.Environment -> Runtime.Scope
global (Runtime.Global scope) = scope
global (Runtime.Local _ parent) = global parent

search :: Text.Text -> Runtime.Environment -> IO (Maybe Runtime.Scope)
search name (Runtime.Global scope) = do
  storage <- IORef.readIORef scope
  if Map.member name storage
    then pure $ Just scope
    else pure Nothing
search name (Runtime.Local scope parent) = do
  storage <- IORef.readIORef scope
  if Map.member name storage
    then pure $ Just scope
    else search name parent

reportError :: Int -> Text.Text -> Interpreter a
reportError loc = Except.throwError . Lox.RuntimeError loc
