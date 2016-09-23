module Render.ANSI
       where

--import           Lang
--import           Parser
--import           Render
--import           Editable
--import           Editable.String
--import qualified Editable.List as EL
--import qualified Data.List     as List
--import           Control.Monad
import           System.Console.ANSI

class PrintANSI a where
  printANSI :: a -> IO ()

class PrintFocusedANSI a where
  printFocusedANSI :: [SGR] -> a -> IO ()

instance PrintANSI a => PrintANSI [a] where
  printANSI xs =
    mapM_ printANSI xs

{-
printANSIE (Left x)  = printANSI x
printANSIE (Right x) = printANSI x

instance PrintANSI Program where
  printANSI (Program n block n') = do
    printANSI n
    printANSI block
    printANSI n'

instance PrintANSI EditableProgram where
  printANSI (EditableProgram _ p) =
    printANSI p

instance PrintANSI Noise where
  printANSI (Noise noises) =
    printANSI noises

{-
instance (PrintANSI a, Editable a) => PrintANSI (EL.List a) where
  printANSI =
    mapM_ printANSI . EL.toList

instance PrintANSI GoodNoise where
  printANSI (Whitespace ces) =
    putStr $ renderString ces
  printANSI (Comment b ces) = do
    setSGR [ SetColor Foreground Dull Yellow
           , SetItalicized True
           --, SetConsoleIntensity FaintIntensity
           ]
    putStr $ "%" ++ renderString ces ++ "\n"
    setSGR [Reset]
-}

instance PrintANSI GoodNoise where
  printANSI (Whitespace s) =
    putStr s
  printANSI (Comment b ces) = do
    setSGR [ SetColor Foreground Dull Yellow
           , SetItalicized True
           --, SetConsoleIntensity FaintIntensity
           ]
    putStr $ "%" ++ renderString ces ++ "\n"
    setSGR [Reset]

instance PrintANSI Block where
  printANSI (Block xs) = do
    setSGR [SetColor Foreground Dull Magenta]
    putStr "{"
    setSGR [Reset]
    mapM_ printANSIE xs
    setSGR [SetColor Foreground Dull Magenta]
    putStr "}"
    setSGR [Reset]

instance PrintANSI Command where
  printANSI (Guarded n g n' xs) = do
    setSGR [SetColor Foreground Dull Cyan]
    putStr "["
    setSGR [Reset]
    printANSI n
    printANSI g
    setSGR [SetColor Foreground Dull Cyan]
    putStr ":"
    setSGR [Reset]
    printANSI n'
    mapM_ printANSIE xs
    setSGR [SetColor Foreground Dull Cyan]
    putStr "]"
    setSGR [Reset]
  printANSI (GuardedGarbage g) = do
    putStr "["
    setSGR [SetColor Background Vivid Red]
    putStr g
    setSGR [Reset]
    putStr "]"
  printANSI (SimpleCommand e n) = do
    printANSI e
    printANSI n
    putStr ";"
  printANSI (SimpleCommandGarbage g) = do
    setSGR [SetColor Background Vivid Red]
    putStr g
    setSGR [Reset]
    putStr ";"
  printANSI (Assignment name n n' e n'') = do
    printANSI name
    setSGR [Reset]
    printANSI n
    putStr "="
    printANSI n'
    printANSI e
    printANSI n''
    putStr ";"
  printANSI (Return n e n') = do
    putStr "^"
    printANSI n
    printANSI e
    printANSI n'
    putStr ";"

instance PrintANSI Guard where
  printANSI (Guard xs) =
    when (not $ null xs)
      (do
          printANSI $ head xs
          mapM_ (\x -> do
                    putStr ","
                    printANSI x
                ) $ tail xs
      )

instance PrintANSI GuardExpr where
  printANSI (GuardExpr an a an' neg bn b bn') = do
    printANSI an
    printANSI a
    printANSI an'
    putStr $ if neg
               then "#"
               else "="
    printANSI bn
    printANSI b
    printANSI bn'

instance PrintANSI Expr where
  printANSI (Expr xs) =
    when (not $ null xs)
      (do
          printANSI $ head xs
          mapM_ (\x -> do
                    putStr "+"
                    printANSI x
                ) $ tail xs
      )

instance PrintANSI SingleExpr where
  printANSI (SingleExpr n b xs) = do
    printANSI n
    printANSI b
    mapM_ printANSIE xs

instance PrintANSI ExprBase where
  printANSI (StringLiteral s) = do
    setSGR [SetColor Foreground Vivid Green]
    putStr $ "\"" ++ escape s ++ "\""
    setSGR [Reset]
  printANSI (BlockExpr b) =
    printANSI b
  printANSI (Reference n) =
    printANSI n
  printANSI (ChildExpr n e n') = do
    putStr "("
    printANSI n
    printANSI e
    printANSI n'
    putStr ")"

instance PrintANSI Selector where
  printANSI (Selector n s) = do
    putStr "."
    printANSI n
    putStr s

instance PrintANSI NameWithLevel where
  printANSI (NameWithLevel name n level) = do
    setSGR [SetColor Foreground Vivid Blue]
    putStr $ concat $ take level $ repeat "*"
    setSGR [Reset]
    printANSI n
    printANSI name

instance PrintANSI Name where
  printANSI (Name s) = do
    setSGR [SetColor Foreground Dull Blue]
    putStr s
    setSGR [Reset]
-}

{-
instance PrintANSI EditableString where
  printANSI (EditableString xs ys) = do
    putStr $ reverse xs
    putStr ys


class PrintFocusedANSI a where
  printFocusedANSI :: [SGR] -> a -> IO ()

instance PrintFocusedANSI EditableString where
  printFocusedANSI sgr (EditableString xs ys) = do
    setSGR sgr
    putStr $ reverse xs
    when
      (not $ null ys)
      (do
          setSGR [SetBlinkSpeed SlowBlink]
          putStr [head ys]
          setSGR [SetBlinkSpeed NoBlink]
          putStr $ tail ys
      )
    setSGR [Reset]
-}
