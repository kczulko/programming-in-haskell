module Excs8 where

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)
-- >>> int2nat 5
-- Succ (Succ (Succ (Succ (Succ Zero))))

int2nat :: Int -> Nat
int2nat n | n <= 0 = Zero
          | otherwise = Succ $ int2nat $ n - 1

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ next) = 1 + nat2int next

-- >>> nat2int . add (int2nat 4) $ int2nat $ 3
-- 7
-- >>> nat2int . add (int2nat 4) $ int2nat $ 0
-- 4
add :: Nat -> Nat -> Nat
add Zero b = b
add (Succ a) b = Succ (add a b)

-- >>> nat2int $ mult (int2nat 3) (int2nat 2)
-- 6
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult a (Succ b) = add a (mult a b)

data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- occurs :: 
