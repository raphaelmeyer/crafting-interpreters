{-# LANGUAGE OverloadedStrings #-}

module Runtime.Environment (Values, assign, define, get, globals, make, pop, push) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Error
import qualified Runtime.Types as Runtime

type Scope = Map.Map Text.Text Runtime.Value

data Values = Global Scope | Local Scope Values

type Interpreter a = Runtime.Interpreter IO Values a

make :: Values
make = Global $ Map.insert "clock" (Runtime.Callable Runtime.Clock) Map.empty

globals :: Interpreter Values
globals = globals' <$> State.get

get :: (Text.Text, Int) -> Interpreter Runtime.Value
get (name, loc) = do
  env <- State.get
  case get' name env of
    Just value -> pure value
    Nothing -> reportError loc $ Text.concat ["Undefined variable '", name, "'."]

define :: Text.Text -> Runtime.Value -> Interpreter ()
define name value = do
  env <- State.get
  State.put $ case env of
    Global scope -> Global $ Map.insert name value scope
    Local scope parent -> Local (Map.insert name value scope) parent

assign :: (Text.Text, Int) -> Runtime.Value -> Interpreter ()
assign (name, loc) value = do
  env <- State.get
  case assign' name value env of
    Just assigned -> State.put assigned
    Nothing -> reportError loc $ Text.concat ["Undefined variable '", name, "'."]

push :: Interpreter ()
push = do
  env <- State.get
  State.put $ Local Map.empty env

pop :: Interpreter ()
pop = do
  env <- State.get
  case env of
    Local _ parent -> State.put parent
    Global _ -> reportError 0 "Can not pop global environment."

globals' :: Values -> Values
globals' env = case env of
  Global scope -> Global scope
  Local _ parent -> globals' parent

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

reportError :: Int -> Text.Text -> Interpreter a
reportError loc = Except.throwError . Error.RuntimeError loc
