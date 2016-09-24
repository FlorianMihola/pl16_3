module Buffer
       where

import           PreludePlus
import           Parser
import           Render
import           Render.Tagged
import           Render.Tagged.Tag as Tag
import qualified Editable          as E
import           Editable.String
import           Editable.List     ( List (..)
                                   , fromList
                                   )
import           Data.Maybe
import           Text.Parsec.Prim  ( runParser )

newtype Buffer = Buffer (List Tagged)
                   deriving (Show)

forward :: Buffer -> Buffer
forward buffer =
  case forwardEditable buffer of
    Just buffer' -> buffer'
    Nothing      -> buffer

backward :: Buffer -> Buffer
backward buffer =
  case backwardEditable buffer of
    Just buffer' -> buffer'
    Nothing      -> buffer

insert :: Buffer -> Char -> Buffer
insert buffer c =
  let
    o = (E.offset buffer) + 1
  in
    case E.insert (addEmpty buffer) c
         >>= Just . renderString
         >>= either
             (\err -> error $ "parser error: " ++ show err) -- (const Nothing)
             (Just . Buffer . fromList . focusFirst . toTagged)
             . runParser program () ""
         >>= (foldl1 (\a b -> \x -> a x >>= b)
                     $ take o $ repeat E.forward
             ) of
      Just buffer' -> buffer'
      Nothing      -> buffer -- error "insert buffer"

--         fromJust $ insertEditable buffer c

addEmpty (Buffer (List xs [])) =
  Buffer $ List xs [Tagged Tag.Unknown True $ fromString ""]
addEmpty buffer =
  buffer

delete :: Buffer -> Buffer
delete buffer =
  case E.remove buffer of
    Just buffer' -> buffer'
    Nothing      -> buffer

backspace :: Buffer -> Buffer
backspace buffer =
  case E.backward buffer >>= E.remove of
    Just buffer' -> buffer'
    Nothing      -> buffer

forwardEditable t@(Buffer l@(List xs ys)) =
  case head' ys >>= E.forward of
    Just yh@(Tagged t _ es) ->
      if E.atEnd yh
        then
          Just $ Buffer $ List ((Tagged t False es) : xs) (focusFirst $ tail ys)
        else
          Just $ Buffer $ List xs (yh : tail ys)
    Nothing ->
      forward' t

backwardEditable t@(Buffer l@(List xs ys)) =
  case head' ys >>= E.backward of
    Just yh ->
      Just $ Buffer $ List xs (yh : tail ys)
    Nothing ->
       backward' t

insertEditable (Buffer l@(List xs ys)) c =
  head' ys >>= flip E.insert c >>= \yh -> Just $ Buffer $ List xs (yh : tail ys)

instance ToString Buffer where
  renderString (Buffer l) =
    renderString l

instance E.Editable Buffer where
  forward = forwardEditable

  backward = backwardEditable

  insert = insertEditable

  remove (Buffer l@(List xs ys)) =
    case head' ys >>= E.remove of
      Just yh -> Just $ Buffer $ List xs (yh : tail ys)
--      Nothing ->

-- if we can't go forward inside (head ys)
forward' :: Buffer -> Maybe Buffer
forward' (Buffer (List xs ys)) =
  head' ys >>= \yh@(Tagged t _ es) ->
                forwardEditable $ Buffer
                                $ List (Tagged t False es : xs)
                                       (focusFirst $ tail ys)

-- if we can't go backward inside (head ys)
backward' :: Buffer -> Maybe Buffer
backward' (Buffer (List xs ys)) =
  tail' xs >>= \xt ->
                let
                  (Tagged t _ es) = head xs
                in
                  backwardEditable $ Buffer
                                   $ List xt
                                          (Tagged t True es : blurFirst ys)


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
