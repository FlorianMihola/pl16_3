module EditableString
       where

import           Editable

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

data ConstrainedEditableString = CEString (Char -> Bool) EditableString

instance Show ConstrainedEditableString where
  show (CEString _ es) =
    "!" ++ show es

instance Editable ConstrainedEditableString where
  forward (CEString c es) =
    forward es >>= Just . CEString c
  backward (CEString c es) =
    backward es >>= Just . CEString c
  insert (CEString constraint es) c =
    if constraint c
      then
        insert es c >>= Just . CEString constraint
      else
        Nothing
