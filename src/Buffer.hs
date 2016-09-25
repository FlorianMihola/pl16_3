module Buffer
       where

import           PreludePlus       hiding ( fromLeft
                                          , fromRight
                                          )
import           Parser
import           Render
import           Render.Tagged
import           Render.Tagged.Tag as Tag
import           Lang
import qualified Editable          as E
import           Editable.String
import           Editable.List     ( List (..)
                                   , fromList
                                   , toList'
                                   , toList
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
         >>= Just . parseBuffer
         >>= (foldl1 (\a b -> \x -> a x >>= b)
                     $ take o $ repeat E.forward
             ) of
      Just buffer' ->
        buffer'
      Nothing ->
        {-Buffer
        $ fromList
        $ focusFirst
        [Tagged Tag.Unparsed True fromString $ renderString buffer]-}
        buffer

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
peek (Buffer l@(List _ ys)) =
  case toList' $ snd $ E.split l of
    [] ->
      Nothing
    xs ->
      (head' $ filter (not . null) $ map (\(Tagged _ _ es) -> toString es) xs)
      >>= head'

  --head' ys >>= (\(Tagged _ _ (EditableString _ ys')) -> head' ys')

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
    >>= \yh ->
         case yh of
           (Tagged t _ e@(EditableString _ [])) ->
             Just $ List (Tagged t False e : xs)
                  $ focusFirst $ tail ys
           _ ->
             Just $ List xs $ focusFirst (yh : tail ys)
         >>= Just . Buffer

  split (Buffer l) =
    let
      (a, b) = E.split l
    in
      (Buffer a, Buffer b)

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
  h : focusFirst ts
focusFirst (Tagged t False es : ts) =
  (Tagged t True es : ts)
focusFirst t =
  t

blurFirst [] =
  []
blurFirst (Tagged t True es : ts) =
  (Tagged t False es : ts)
blurFirst (h@(Tagged t False es) : ts) =
  h : blurFirst ts

parseBuffer s =
  if null s
    then
      Buffer $ fromList $ [Tagged Tag.Unknown True $ fromString ""]
    else
      case runParser program () "" s of
        Left err ->
          Buffer $ fromList $ [Tagged Tag.Unparsed True $ fromString s]
        Right buffer ->
          Buffer $ fromList $ focusFirst $ toTaggedA [] buffer

append (Buffer a) (Buffer b) =
  let
    la = toList' a
    lb = toList' b
  in
    Buffer $ fromList $ la ++ lb
    --error $ (show a) ++ (show $ E.toEnd a) ++ (show $ E.toBeginning a)

addCursor buffer@(Buffer l) =
  let
    (a, b) = E.split buffer
  in
    case (peek b, E.forward b) of
      (Just c, Just b') ->
        append (append
                  a
                  (Buffer $ fromList
                            [Tagged Tag.Cursor False
                             $ fromString
                             $ if c == '\n'
                                 then
                                   [' ', c]
                                 else
                                   [c]
                            ]
                  )
               )
               $ snd $ E.split b'
      (Just c, Nothing) ->
        error $ "Just " ++ show c ++ " Nothing @ addCursor"
      (Nothing, Just b') ->
        error $ "Nothing Just " ++ show b' ++ " @ addCursor"
      _ ->
        append
          a
          (Buffer $ fromList
                    [Tagged Tag.Cursor False $ fromString " "]
          )

hasCursor (Buffer l) =
  any hasCursor' $ toList l
  where
    hasCursor' (Tagged Tag.Cursor _ _) = True
    hasCursor' _ = False

toLines buffer =
  let
    (a, b) = E.split $ forward $ fst $ forwardUntil (== '\n') $ E.toBeginning buffer
  in
    case E.forward b of
      Just b' ->
        a : toLines b
      Nothing ->
        [a, b]

bufferLength =
  length . renderString

splitLine pre n buffer =
  if bufferLength buffer <= n
    then
      [buffer]
    else
      let
        (a, b) = E.split $ nTimes (n - 1) forward $ E.toBeginning buffer
      in
        (append a $ Buffer
                  $ fromList [Tagged Tag.Whitespace False $ fromString "\n"]
        )
        :
        (splitLine pre n
         $ append (Buffer
                   $ fromList [Tagged Tag.Padding False $ fromString pre]
                  )
         b
        )
