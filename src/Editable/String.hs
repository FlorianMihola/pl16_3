module Editable.String
       where

import           Editable
import           Render
import           Render.ANSI
import           System.Console.ANSI
import           Control.Monad

data EditableString = EditableString String String

fromString :: String -> EditableString
fromString s =
  EditableString "" s

toString :: EditableString -> String
toString (EditableString a b) =
  reverse a ++ b

instance Show EditableString where
  show (EditableString a b) =
    "\"" ++ reverse a ++ "\"" ++ b ++ "\""

instance Editable EditableString where
  forward (EditableString a b) =
    if null b
      then
        Nothing
      else
        Just $ EditableString (head b : a) (tail b)

  backward (EditableString a b) =
    if null a
      then
        Nothing
      else
        Just $ EditableString (tail a) (head a : b)

  insert (EditableString a b) c =
    Just $ EditableString (c : a) b

  remove (EditableString "" "") =
    Nothing
  remove (EditableString a b) =
    Just $ EditableString a $ tail b

instance ToString EditableString where
  renderString = toString

instance PrintANSI EditableString where
  printANSI (EditableString xs ys) = do
    putStr $ reverse xs
    putStr ys

instance PrintFocusedANSI EditableString where
  printFocusedANSI sgr (EditableString xs ys) = do
    setSGR sgr
    putStr $ reverse xs
    when
      (not $ null ys)
      (do
          setSGR [SetBlinkSpeed SlowBlink, SetSwapForegroundBackground True]
          putStr [head ys]
          setSGR [SetBlinkSpeed NoBlink, SetSwapForegroundBackground False]
          putStr $ tail ys
      )
    setSGR [Reset]
