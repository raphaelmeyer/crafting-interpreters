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
      { eMessage :: Text.Text
      }
  deriving (Eq)

instance Show Error where
  show err = case err of
    ScanError line m ->
      "Scan Error in line "
        ++ show line
        ++ ": "
        ++ Text.unpack m
    ParseError line lexeme m ->
      "Parse Error in line "
        ++ show line
        ++ ": at '"
        ++ Text.unpack lexeme
        ++ "' "
        ++ Text.unpack m
    RuntimeError m -> "Runtime Error: " ++ Text.unpack m
