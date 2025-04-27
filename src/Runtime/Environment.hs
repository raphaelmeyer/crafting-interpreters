{-# LANGUAGE OverloadedStrings #-}

module Runtime.Environment (Environment, assign, define, get, globals, make, pop, push) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Error
import qualified Runtime.Types as Runtime

type Environment = State.StateT Runtime.Values

type Environment' m a = Except.ExceptT Error.Error (Environment m) a

make :: Runtime.Values
make = Runtime.Global $ Map.insert "clock" (Runtime.Callable Runtime.Clock) Map.empty

globals :: (Monad m) => Environment m Runtime.Values
globals = globals' <$> State.get

get :: (Monad m) => (Text.Text, Int) -> Environment' m Runtime.Value
get (name, loc) = do
  env <- State.get
  case get' name env of
    Just value -> pure value
    Nothing -> reportError loc $ Text.concat ["Undefined variable '", name, "'."]

define :: (Monad m) => Text.Text -> Runtime.Value -> Environment m ()
define name value = do
  env <- State.get
  State.put $ case env of
    Runtime.Global scope -> Runtime.Global $ Map.insert name value scope
    Runtime.Local scope parent -> Runtime.Local (Map.insert name value scope) parent

assign :: (Monad m) => (Text.Text, Int) -> Runtime.Value -> Environment' m ()
assign (name, loc) value = do
  env <- State.get
  case assign' name value env of
    Just assigned -> State.put assigned
    Nothing -> reportError loc $ Text.concat ["Undefined variable '", name, "'."]

push :: (Monad m) => Environment m ()
push = do
  env <- State.get
  State.put $ Runtime.Local Map.empty env

pop :: (Monad m) => Environment' m ()
pop = do
  env <- State.get
  case env of
    Runtime.Local _ parent -> State.put parent
    Runtime.Global _ -> reportError 0 "Can not pop global environment."

globals' :: Runtime.Values -> Runtime.Values
globals' env = case env of
  Runtime.Global scope -> Runtime.Global scope
  Runtime.Local _ parent -> globals' parent

get' :: Text.Text -> Runtime.Values -> Maybe Runtime.Value
get' name env = case env of
  Runtime.Global scope -> Map.lookup name scope
  Runtime.Local scope parent -> case Map.lookup name scope of
    Just value -> Just value
    Nothing -> get' name parent

assign' :: Text.Text -> Runtime.Value -> Runtime.Values -> Maybe Runtime.Values
assign' name value (Runtime.Global scope) = case Map.lookup name scope of
  Just _ -> Just . Runtime.Global $ Map.insert name value scope
  Nothing -> Nothing
assign' name value (Runtime.Local scope parent) = case Map.lookup name scope of
  Just _ -> Just $ Runtime.Local (Map.insert name value scope) parent
  Nothing -> case assign' name value parent of
    Just assigned -> Just $ Runtime.Local scope assigned
    Nothing -> Nothing

reportError :: (Monad m) => Int -> Text.Text -> Environment' m a
reportError loc = Except.throwError . Error.RuntimeError loc
