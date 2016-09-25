--{-# LANGUAGE FlexibleInstances #-}

module Render.Tagged
       where

import           PreludePlus
import           Render
import           Render.ANSI
import           Render.Curses
import qualified Render.Tagged.Tag   as Tag
import           Render.Tagged.Tag   ( Tag
                                     , tagSGR
                                     )
import           Editable
import           Editable.String
import           Editable.List       ( List (..) )
import           System.Console.ANSI
import           UI.NCurses

data Tagged = Tagged Tag Bool EditableString

instance Show Tagged where
  show (Tagged tag focused es) =
    "[" ++ (if focused then "!" else "") ++ show tag ++ ":" ++ show es ++ "]"

instance ToString Tagged where
  renderString (Tagged _ _ es) =
    renderString es

instance Editable Tagged where
  forward (Tagged t f es) =
    forward es >>= Just . Tagged t f
  backward (Tagged t f es) =
    backward es >>= Just . Tagged t f
  insert (Tagged t f es) c =
    insert es c >>= Just . Tagged t f
  remove (Tagged t f es) =
    remove es >>= Just . Tagged t f
  split (Tagged t f es) =
    let
      (a, b) = split es
    in
      (Tagged t False a, Tagged t False b)

instance PrintANSI Tagged where
  printANSI (Tagged tag focused es) = do
    setSGR $ tagSGR tag
    if focused
      then
        printFocusedANSI (tagSGR tag) es
      else
        printANSI es
    setSGR [Reset]

class ToTagged a where
  toTagged :: a -> [Tagged]

instance ToTagged a => ToTagged [a] where
  toTagged =
    concat . map toTagged

instance (ToTagged a, ToTagged b) => ToTagged (Either a b) where
  toTagged (Left x) =
    toTagged x
  toTagged (Right x) =
    toTagged x
