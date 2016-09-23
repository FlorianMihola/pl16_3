module Parser.Char
       where

import           Parser.Type
import           Text.Parsec.Char
import qualified Data.List              as List

nameChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

escapeChar = '\\' -- not in spec

escapableChars = "\""

escaped :: [Char] -> Parser Char
escaped escapableChars = do
  char escapeChar
  oneOf escapableChars

escape :: String -> String
escape = List.foldr (\c s -> if List.elem c escapableChars
                             then escapeChar : c : s
                             else c : s
                    )
         ""
