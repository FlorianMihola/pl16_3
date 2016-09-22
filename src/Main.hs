module Main
       where

import           Text.Parsec.Prim       ( runParser )
import           Text.Parsec.String     ( parseFromFile )
import           Parser
import           Render
import           Render.ANSI
import           Editable
import           EditableString
import           System.Environment

main = do
  args <- getArgs
  if length args == 1
    then
      do
        e <- parseFromFile program (args !! 0)
        case e of
          Left err ->
            putStrLn $ "Parse error: " ++ show err
          Right p ->
            printANSI p
    else
      putStrLn "Please specify which file to read."
