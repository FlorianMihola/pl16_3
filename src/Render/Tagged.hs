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

{-instance PrintCurses Tagged where
  printCurses (Tagged tag focused es) = do
    tagColorID tag >>= setColor
    if focused
      then
        printFocusedCurses (tagColorID tag) es
      else
        printCurses es
    setColor defaultColorID
-}

instance Editable Tagged where
  forward (Tagged t f es) =
    forward es >>= Just . Tagged t f
  backward (Tagged t f es) =
    backward es >>= Just . Tagged t f
  insert (Tagged t f es) c =
    insert es c >>= Just . Tagged t f
  remove (Tagged t f es) =
    remove es >>= Just . Tagged t f

instance PrintANSI Tagged where
  printANSI (Tagged tag focused es) = do
    setSGR $ tagSGR tag
    if focused
      then
        printFocusedANSI (tagSGR tag) es
      else
        printANSI es
    setSGR [Reset]

{-
newtype TaggedList = TaggedList (List Tagged)
                   deriving (Show)

instance ToString TaggedList where
  renderString (TaggedList l) =
    renderString l

instance Editable TaggedList where
  forward t@(TaggedList l@(List xs ys)) = -- TODO: jump over empty Tagged
    case head' ys >>= forward of
      Just yh@(Tagged t _ es) ->
        if atEnd yh
          then
            Just $ TaggedList $ List ((Tagged t False es) : xs) (focusFirst $ tail ys)
          else
            Just $ TaggedList $ List xs (yh : tail ys)
      Nothing ->
        forward' t

  backward t@(TaggedList l@(List xs ys)) =
    case head' ys >>= backward of
      Just yh ->
        Just $ TaggedList $ List xs (yh : tail ys)
      Nothing ->
        backward' t

  insert (TaggedList l@(List xs ys)) c =
    head' ys >>= flip insert c >>= \yh -> Just $ TaggedList $ List xs (yh : tail ys)

  remove (TaggedList l@(List xs ys)) =
    case head' ys >>= remove of
      Just yh -> Just $ TaggedList $ List xs (yh : tail ys)
--      Nothing ->

-- if we can't go forward inside (head ys)
forward' :: TaggedList -> Maybe TaggedList
forward' (TaggedList (List xs ys)) =
  head' ys >>= \yh@(Tagged t _ es) ->
                forward $ TaggedList
                        $ List (Tagged t False es : xs) (focusFirst $ tail ys)

-- if we can't go backward inside (head ys)
backward' :: TaggedList -> Maybe TaggedList
backward' (TaggedList (List xs ys)) =
  tail' xs >>= \xt -> let
                        (Tagged t _ es) = head xs
                      in
                        backward $ TaggedList
                                 $ List xt (Tagged t True es : blurFirst ys)

instance PrintANSI TaggedList where
  printANSI (TaggedList l) = do
    printANSI l


focusFirst [] =
  []
focusFirst (h@(Tagged t False (EditableString [] [])): ts) =
  check $ h : focusFirst ts
focusFirst (Tagged t False es : ts) =
  check $ (Tagged t True es : ts)
focusFirst t =
  check t

blurFirst [] =
  []
blurFirst (Tagged t True es : ts) =
  (Tagged t False es : ts)
blurFirst (h@(Tagged t False es) : ts) =
  h : blurFirst ts

check xs =
  let
    focused = filter (\(Tagged _ f _) -> f) xs
  in
    if length focused == 1
      then
        xs
      else
        error $ "check " ++ show focused

-}

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
