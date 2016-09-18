module Main where

import           Text.Parsec.Prim       ( runParser )
import           Text.Parsec.String     ( parseFromFile )
import           Parser
import           System.Environment

main = do
  args <- getArgs
  if length args == 1
    then
      do
        e <- parseFromFile exprBase (args !! 0)
        case e of
          Left err ->
            putStrLn $ "Parse error: " ++ show err
          Right result ->
            putStrLn $ show result
    else
      putStrLn "Please specify which file to read."
