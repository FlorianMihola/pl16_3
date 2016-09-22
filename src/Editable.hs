module Editable
       where

class Editable a where
  forward :: a -> Maybe a
  backward :: a -> Maybe a
  insert :: a -> Char -> Maybe a
