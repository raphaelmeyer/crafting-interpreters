{-# LANGUAGE OverloadedStrings #-}

module Environment (Environment, assign, define, empty, get) where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Error
import qualified Lox

type Values = Map.Map Text.Text Lox.Value

type Environment = State.StateT Values

empty :: Values
empty = Map.empty

get :: (Monad m) => Text.Text -> Except.ExceptT Error.Error (Environment m) Lox.Value
get name = do
  env <- State.get
  case Map.lookup name env of
    Just value -> pure value
    Nothing -> Except.throwError . Error.Error 0 $ Text.concat ["Undefined variable '", name, "'."]

define :: (Monad m) => Text.Text -> Lox.Value -> Environment m ()
define name value = do
  State.modify $ Map.insert name value

assign :: (Monad m) => Text.Text -> Lox.Value -> Except.ExceptT Error.Error (Environment m) ()
assign name value = do
  env <- State.get
  case Map.lookup name env of
    Just _ -> State.modify $ Map.insert name value
    Nothing -> Except.throwError . Error.Error 0 $ Text.concat ["Undefined variable '", name, "'."]
