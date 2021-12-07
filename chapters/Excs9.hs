module Excs9 where

-- >>> subs [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = currentSubs ++ map (x:) currentSubs
  where currentSubs = subs xs
