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
--         >>= error . show
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


down :: Buffer -> Buffer
down buffer =
  let
    column' = fromLeft buffer
    nextLine = fst $ forwardUntil (== '\n') $ forward buffer
    nextLineLen = fromRight $ forward nextLine
    column = min column' nextLineLen
  in
    (if column > 0
      then
        foldl1 (.) $ take column $ repeat forward
      else
        id
    )
    nextLine

up :: Buffer -> Buffer
up buffer =
  let
    column' = fromLeft buffer
    prevLine = fst $ backwardUntil (== '\n') $ fst $ backwardUntil (/= '\n') buffer
    prevLineLen = fromLeft $ backward prevLine
    column = prevLineLen - column' + 1
  in
    (if column > 0
       then
         foldl1 (.) $ take column $ repeat backward
       else
         id
    )
    prevLine


peek :: Buffer -> Maybe Char
peek (Buffer (List _ ys)) =
  head' ys >>= (\(Tagged _ _ (EditableString _ ys')) -> head' ys')

forwardUntil :: (Char -> Bool) -> Buffer -> (Buffer, Int)
forwardUntil p b =
  let
    f i p buffer =
      if peek buffer == Just '\n'
        then
          (buffer, i)
        else
          case E.forward buffer of
            Just buffer' ->
              f (i + 1) p $ buffer'
            Nothing ->
              (buffer, i)
  in
    f 0 p b

backwardUntil :: (Char -> Bool) -> Buffer -> (Buffer, Int)
backwardUntil p b =
  let
    f i p buffer =
      if peek buffer == Just '\n'
        then
          (buffer, i)
        else
          case E.backward buffer of
            Just buffer' ->
              f (i + 1) p $ buffer'
            Nothing ->
              (buffer, i)
  in
    f 0 p b

fromLeft :: Buffer -> Int
fromLeft buffer =
  let
    (buffer', i) = backwardUntil (== '\n') buffer
  in
    if peek buffer' == Just '\n'
      then i
      else i + 1

fromRight :: Buffer -> Int
fromRight buffer =
  let
    (buffer', i) = forwardUntil (== '\n') buffer
  in
    if peek buffer' == Just '\n'
      then i
      else i + 1

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
    head' ys
    >>= E.remove
    >>= \yh -> Just $ Buffer $ List xs (yh : tail ys)

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
