{-# LANGUAGE OverloadedStrings #-}

module Token where

import qualified Data.Text as Text

data TokenType
  = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | SemiColon
  | Slash
  | Star
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | Identifier Text.Text
  | String Text.Text
  | Integer Int
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | EOF
  deriving (Eq, Show)

data Token = Token
  { tType :: TokenType,
    tLexeme :: Text.Text,
    tLine :: Int
  }
  deriving (Eq)

instance Show Token where
  show token = (show . tType $ token) ++ ": " ++ (Text.unpack . tLexeme $ token)
