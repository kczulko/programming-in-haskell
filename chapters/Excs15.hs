{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Excs15 where

------------------------------
-- 2.

-- fst (1+2, 2+3)
-- 1+2
-- 3

-- 3.
-- mult = \x -> (\y -> x * y)

-- mult 3 4
-- applying mult
-- (\x -> (\y -> x * y)) 3 4
-- # applying 3
-- (\y -> 3 * y) 4
-- # applying 4
-- 3 * 4
-- 12     


-- >>> take 8 fibs
-- [1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = [x + y | (x,y) <- pattern `zip` tail pattern]
       where
         pattern = 0:1:fibs

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

repeat' :: a -> Tree a
repeat' a = Node (repeat' a) a (repeat' a)

take' :: Int -> Tree a -> Tree a
take' n t | n <= 0 = Leaf
          | otherwise = case t of
              Leaf -> Leaf
              Node l a r -> Node (take' (n - 1) $! l) a (take' (n - 1) $! r)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

-- >>> replicate' 3 Leaf
-- Node (Node (Node Leaf Leaf Leaf) Leaf (Node Leaf Leaf Leaf)) Leaf (Node (Node Leaf Leaf Leaf) Leaf (Node Leaf Leaf Leaf))

-- >>> sqroot 6
-- 2.4494943716069653

sqroot :: Double -> Double
sqroot n = fst $ last $ takeWhile ((>distance) . snd) $ iterate next (initial, initial)
 where
   initial = 1.0
   distance = 0.00001
   next (a,_) = let
                  new = (a + n/a) / 2
                in
                  (new, abs (new - a))

