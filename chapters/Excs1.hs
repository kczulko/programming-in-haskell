module Excs1 where

product :: Num a => [a] -> a
product = foldr (*) 1

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort bigger
  where
    smaller = [a | a <- xs, a <= x]
    bigger = [b | b <- xs, b > x]
