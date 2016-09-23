module PreludePlus
       where

head' :: [a] -> Maybe a
head' xs =
  if null xs
    then
      Nothing
    else
      Just $ head xs

tail' :: [a] -> Maybe [a]
tail' xs =
  if null xs
    then
      Nothing
    else
      Just $ tail xs
