module Editable.List
       where

import           PreludePlus
import           Editable
import           Render
import           Render.ANSI

data List a = List [a] [a]
            deriving (Show)

fromList :: [a] -> List a
fromList xs =
  List [] xs

toList :: Editable a => List a -> [a]
toList (List xs ys) =
 reverse xs ++ ys

toList' :: Editable a => List a -> [a]
toList' (List xs ys) =
  map toBeginning (reverse xs ++ ys)

instance Editable a => Editable (List a) where
  forward l@(List xs ys) =
    case head' ys >>= forward of
      Just yh ->
        if atEnd yh
          then
            Just $ List (yh : xs) (tail ys)
          else
            Just $ List xs (yh : tail ys)
      Nothing ->
        forward' l

  backward l@(List xs ys) =
    case head' ys >>= backward of
      Just yh ->
        Just $ List xs (yh : tail ys)
      Nothing ->
        backward' l
  insert _ _ = Nothing

instance (ToString a, Editable a) => ToString (List a) where
  renderString =
    concat . map renderString . toList

instance (PrintANSI a, Editable a) => PrintANSI (List a) where
  printANSI =
    mapM_ printANSI . toList

-- if we can't go forward inside (head xs)
forward' :: Editable a => List a -> Maybe (List a)
forward' (List xs ys) =
  head' ys >>= \yh -> forward $ List (yh : xs) (tail ys)

-- if we can't backward inside (head ys)
backward' :: Editable a => List a -> Maybe (List a)
backward' (List xs ys) =
  tail' xs >>= \xt -> backward $ List xt (head xs : ys)
