{-# LANGUAGE OverloadedStrings #-}

module Environment (Environment, assign, define, get, make, pop, push) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Error
import qualified Runtime

type Scope = Map.Map Text.Text Runtime.Value

data Values = Global Scope | Local Scope Values

type Environment = State.StateT Values

make :: Values
make = Global $ Map.insert "clock" (Runtime.Callable Runtime.Clock) Map.empty

get :: (Monad m) => Text.Text -> Except.ExceptT Error.Error (Environment m) Runtime.Value
get name = do
  env <- State.get
  case get' name env of
    Just value -> pure value
    Nothing -> reportError $ Text.concat ["Undefined variable '", name, "'."]

define :: (Monad m) => Text.Text -> Runtime.Value -> Environment m ()
define name value = do
  env <- State.get
  State.put $ case env of
    Global scope -> Global $ Map.insert name value scope
    Local scope parent -> Local (Map.insert name value scope) parent

assign :: (Monad m) => Text.Text -> Runtime.Value -> Except.ExceptT Error.Error (Environment m) ()
assign name value = do
  env <- State.get
  case assign' name value env of
    Just assigned -> State.put assigned
    Nothing -> reportError $ Text.concat ["Undefined variable '", name, "'."]

push :: (Monad m) => Environment m ()
push = do
  env <- State.get
  State.put $ Local Map.empty env

pop :: (Monad m) => Except.ExceptT Error.Error (Environment m) ()
pop = do
  env <- State.get
  case env of
    Local _ parent -> State.put parent
    Global _ -> reportError "Can not pop global environment."

get' :: Text.Text -> Values -> Maybe Runtime.Value
get' name env = case env of
  Global scope -> Map.lookup name scope
  Local scope parent -> case Map.lookup name scope of
    Just value -> Just value
    Nothing -> get' name parent

assign' :: Text.Text -> Runtime.Value -> Values -> Maybe Values
assign' name value (Global scope) = case Map.lookup name scope of
  Just _ -> Just . Global $ Map.insert name value scope
  Nothing -> Nothing
assign' name value (Local scope parent) = case Map.lookup name scope of
  Just _ -> Just $ Local (Map.insert name value scope) parent
  Nothing -> case assign' name value parent of
    Just assigned -> Just $ Local scope assigned
    Nothing -> Nothing

reportError :: (Monad m) => Text.Text -> Except.ExceptT Error.Error (Environment m) a
reportError = Except.throwError . Error.RuntimeError
