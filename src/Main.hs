module Main
       where

import           Text.Parsec.Prim       ( runParser )
import           Text.Parsec.String     ( parseFromFile )
import           Parser
import           Render
import           Render.ANSI
import           Editable
import           Editable.String
import           Editable.List
--import           Editable.Instances
import           Render.Tagged
import           System.Environment
import           Data.Maybe

main = do
  args <- getArgs
  if length args == 1
    then
      do
        e <- parseFromFile program (args !! 0)
        case e of
          Left err ->
            putStrLn $ "Parse error: " ++ show err
          Right p -> do
            let tagged = toTagged p
            --print p
            let tagged' = fromJust $ (Just $ TaggedList $ fromList $ focusFirst tagged)
            print tagged'
            printANSI tagged'
    else
      putStrLn "Please specify which file to read."
