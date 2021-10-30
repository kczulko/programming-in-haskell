module Excs6 where

--------------------------------------------------------------------------------
-- recursion excercises
--------------------------------------------------------------------------------

-- >>> euclid 6 27
-- 3
euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a > b = euclid (a-b) b
           | otherwise = euclid a (b-a)

-- >>> fac 3
-- 6
fac :: Int -> Int
fac n | n < 0 = error $ "Negative integer passed: " <> show n
      | n == 0 = 1
      | otherwise = n * fac (n-1)

-- >>> sumdown (-2)
-- -2
-- >>> sumdown 3
-- 6
sumdown :: Int -> Int
sumdown n | n > 0 = n + sumdown (n-1)
          | n == 0 = 0
          | otherwise =  n

-- >>> 2 ^^^ 3
-- 8
(^^^) :: (Num a, Eq a, Ord a) => a -> a -> a
a ^^^ b | b > 0 = a * (a ^^^ (b-1))
        | b == 0 = 1
        | otherwise = a

-- >>> and' [True, False, True]
-- False

-- >>> and' [True, True, True]
-- True
and' :: [Bool] -> Bool
and' = foldr (&&) True

-- >>> concat' [[1,2], [5], [6,7]]
-- [1,2,5,6,7]
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- >>> replicate' 4 4
-- [4,4,4,4]
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x

-- >>> [1,2,3,4,5,6] !!! 4
-- No such element
(!!!) :: [a] -> Int -> a
[] !!! _ = error "No such element"
(x:xs) !!! n | n == 0 = x
             | otherwise = xs !!! (n-1)


-- >>> elem' 3 [1,6,2,7]
-- False
-- >>> elem' 3 [1,6,2,7, 3]
-- True
elem' :: Eq a => a -> [a] -> Bool
elem' a (x:xs) | a == x = True
               | otherwise = elem' a xs
elem' _ _ = False


-- >>> merge [1,3,5] [2,4,6]
-- [1,2,3,4,5,6]
merge :: Ord a => [a] -> [a] -> [a]
merge xs []   = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x >= y = y : x : merge xs ys
                    | otherwise = x : y : merge xs ys

-- >>> msort [4,1,6,2,8]
-- [1,2,4,6,8]
msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort other = merge left right
  where left = msort firstHalve
        right = msort secondHalve
        (firstHalve, secondHalve) = splitAt halfWayOfOther other
        halfWayOfOther = length other `div` 2

-- >>> sum' [1,2,3,4]
-- 10
sum' :: Num a => [a] -> a
sum' = foldr (+) 0

-- >>> take' 3 [1..]
-- [1,2,3]
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n l@(x:xs) | n > 0 = x : take' (n-1) xs
                 | n == 0 = []
                 | otherwise = l

-- >>> last' [1..5]
-- 5
-- >>> last' []
-- No such element
-- >>> last' [1]
-- 1
last' :: [a] -> a
last' [] = error "No such element"
last' [x] = x
last' (x:xs) = last' xs
