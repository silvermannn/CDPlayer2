module Data.Common where

deleteN :: Int -> [a] -> [a]
deleteN n xs = as ++ drop 1 bs
  where
    (as, bs) = splitAt n xs

replaceN :: Int -> a -> [a] -> [a]
replaceN n x xs = x : deleteN n xs

zipFull :: Ord a => (a -> b) -> (a -> a -> b) -> [a] -> [a] -> [b]
zipFull f _ xs [] = map f xs
zipFull f _ [] ys = map f ys
zipFull f g (x:xs) (y:ys) =
  case compare x y of
    LT -> f x : zipFull f g xs (y : ys)
    EQ -> g x y : zipFull f g xs ys
    GT -> f y : zipFull f g (x : xs) ys
