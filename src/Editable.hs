module Editable
       where

import           Data.Maybe

class Editable a where
  -- there are default implementations for forward, backward, insert and remove
  -- but they should be overridden
  -- TODO: remove default implementations once all proper implementations
  --       are done
  forward :: a -> Maybe a
  forward _ = Nothing

  backward :: a -> Maybe a
  backward _ = Nothing

  insert :: a -> Char -> Maybe a
  insert _ _ = Nothing

  remove :: a -> Maybe a
  remove _ = Nothing

  -- for the following the default implementations should do
  atBeginning :: a -> Bool
  atBeginning = isNothing . backward

  atEnd :: a -> Bool
  atEnd = isNothing . forward

  toBeginning :: a -> a
  toBeginning x =
    if atBeginning x
      then
        x
      else
        toBeginning $ fromJust $ backward x

  toEnd :: a -> a
  toEnd x =
    if atEnd x
      then
        x
      else
        toEnd $ fromJust $ forward x

  offset :: a -> Int
  offset x =
    let
      offset' i x =
        if atBeginning x
        then
          i
        else
          offset' (i + 1) $ fromJust $ backward x
    in
      offset' 0 x
