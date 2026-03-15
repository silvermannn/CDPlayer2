module Data.Common where

deleteN :: Int -> [a] -> [a]
deleteN n xs = as ++ drop 1 bs
  where
    (as, bs) = splitAt n xs

replaceN :: Int -> a -> [a] -> [a]
replaceN n x xs = x : deleteN n xs
