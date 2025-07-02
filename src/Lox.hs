module Lox where

import qualified Data.Text as Text

type Result a = Either [Error] a

data Error
  = ScanError
      { eLine :: Int,
        eMessage :: Text.Text
      }
  | ParseError
      { eLine :: Int,
        eLexeme :: Text.Text,
        eMessage :: Text.Text
      }
  | ResolveError
      { eLine :: Int,
        eLexeme :: Text.Text,
        eMessage :: Text.Text
      }
  | RuntimeError
      { eLine :: Int,
        eMessage :: Text.Text
      }
  deriving (Eq)

instance Show Error where
  show err = case err of
    ScanError line message ->
      "Scanner [line "
        ++ show line
        ++ "] Error: "
        ++ Text.unpack message
    ParseError line lexeme message ->
      "Parser [line "
        ++ show line
        ++ "] Error at "
        ++ Text.unpack lexeme
        ++ ": "
        ++ Text.unpack message
    ResolveError line lexeme message ->
      "Resolver [line "
        ++ show line
        ++ "] Error at '"
        ++ Text.unpack lexeme
        ++ "': "
        ++ Text.unpack message
    RuntimeError line message ->
      Text.unpack message
        ++ "\n[line "
        ++ show line
        ++ "]"

this :: Text.Text
this = Text.pack "this"

super :: Text.Text
super = Text.pack "super"

initializer :: Text.Text
initializer = Text.pack "init"
