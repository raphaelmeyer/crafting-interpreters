module Error where

import qualified Data.Text as Text

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
        ++ "] Error at '"
        ++ Text.unpack lexeme
        ++ "': "
        ++ Text.unpack message
    RuntimeError line message ->
      Text.unpack message
        ++ "\n[line "
        ++ show line
        ++ "]"
