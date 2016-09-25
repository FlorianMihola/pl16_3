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

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

isLeft :: Either a b -> Bool
isLeft = not . isRight

fromRight :: Either a b -> b
fromRight (Right x) = x

fromLeft :: Either a b -> a
fromLeft (Left x) = x

nTimes 0 _ = id
nTimes n f =
  foldl1 (.) $ take n $ repeat f

nTimes' 0 _ = return
nTimes' n f =
  foldl1 (\a b -> \x -> a x >>= b) $ take n $ repeat f
