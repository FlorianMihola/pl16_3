module Render.ANSI
       where

import           System.Console.ANSI

class PrintANSI a where
  printANSI :: a -> IO ()

class PrintFocusedANSI a where
  printFocusedANSI :: [SGR] -> a -> IO ()

instance PrintANSI a => PrintANSI [a] where
  printANSI xs =
    mapM_ printANSI xs
