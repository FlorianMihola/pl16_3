--{-# LANGUAGE FlexibleInstances #-}

module Render.Tagged
       where

import           PreludePlus
--import           Lang
import           Render
import           Render.ANSI
import qualified Render.Tagged.Tag   as Tag
import           Render.Tagged.Tag   ( Tag
                                     , tagSGR
                                     )
import           Editable
import           Editable.String
import           Editable.List       ( List (..) )
import           System.Console.ANSI

data Tagged = Tagged Tag Bool EditableString

instance Show Tagged where
  show (Tagged tag _ es) =
    "[" ++ show tag ++ ":" ++ show es ++ "]"

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

instance PrintANSI Tagged where
  printANSI (Tagged tag focused es) = do
    setSGR $ tagSGR tag
    if focused
      then
        printFocusedANSI (tagSGR tag) es
      else
        printANSI es
    setSGR [Reset]


newtype TaggedList = TaggedList (List Tagged)
                   deriving (Show)

instance Editable TaggedList where
  forward t@(TaggedList l@(List xs ys)) =
    case head' ys >>= forward of
      Just yh@(Tagged t _ es) ->
        if atEnd yh
          then
            Just $ TaggedList $ List ((Tagged t False es) : xs) (focusFirst $ tail ys)
          else
            Just $ TaggedList $ List xs (yh : tail ys)
      Nothing ->
        forward' t

  backward t@(TaggedList l@(List xs ys)) = -- TODO
    case head' ys >>= backward of
      Just yh ->
        Just $ TaggedList $ List xs (yh : tail ys)
      Nothing ->
        backward' t
  insert _ _ = Nothing

-- if we can't go forward inside (head xs)
forward' :: TaggedList -> Maybe TaggedList
forward' (TaggedList (List xs ys)) =
  head' ys >>= \yh@(Tagged t _ es) ->
                forward $ TaggedList
                        $ List (Tagged t False es : xs) (focusFirst $ tail ys)

-- if we can't backward inside (head ys)
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

focusFirst [] =
  []
focusFirst (Tagged t False es : ts) =
  (Tagged t True es : ts)

blurFirst [] =
  []
blurFirst (Tagged t True es : ts) =
  (Tagged t False es : ts)

{-
instance ToTagged ConstrainedEditableString where
  toTagged (CEString True es) =
    (
-}
