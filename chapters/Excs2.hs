module Excs2 where

last1 :: [a] -> a
last1 = head . reverse

last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)

init1 :: [a] -> [a]
init1 = reverse . tail . reverse

init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs
