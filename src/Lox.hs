module Lox where

import qualified Error

type Result a = Either [Error.Error] a
