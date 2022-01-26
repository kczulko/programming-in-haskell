module Excs9 where

import Data.List (partition)

-- >>> subs [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = currentSubs ++ map (x :) currentSubs
  where
    currentSubs = subs xs

-- >>> interleave 1 [2,3,4]
-- [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concatMap (interleave x) (perms xs)

-- >>> choices [1,2,3]
-- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
choices :: [a] -> [[a]]
choices = concatMap perms . subs

-- >>> choices2 [1,2,3]
-- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
choices2 :: [a] -> [[a]]
choices2 a = [y | x <- subs a, y <- perms x]

-- >>> isChoice [1,2,3] []
-- False
-- >>> isChoice [1,2,3] [3]
-- False
-- >>> isChoice [1,2,3] [3,2,1,6]
-- False
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (_ : _) [] = False
isChoice (x : xs) ys = case partition (== x) ys of
  ([], _) -> False
  _ -> isChoice xs ys
