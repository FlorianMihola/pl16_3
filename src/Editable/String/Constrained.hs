module Editable.String.Constrained
       where

import           Editable
import qualified Editable.String as ES
import           Editable.String ( EditableString (..) )
import           Render

data ConstrainedEditableString = CEString (Char -> Bool) EditableString

editableString :: ConstrainedEditableString -> EditableString
editableString (CEString _ es) = es

toString :: ConstrainedEditableString -> String
toString =
  ES.toString . editableString

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
  split (CEString c es) =
    let
      (a, b) = split es
    in
      (CEString c a, CEString c b)

instance ToString ConstrainedEditableString where
  renderString (CEString _ es) = renderString es
