module Excs4 where

halve :: [a] -> ([a], [a])
halve a = (firstHalf, secondHalf)
  where
    halfIndx = length a `div` (2 :: Int)
    firstHalf = take halfIndx a
    secondHalf = drop halfIndx a

-- ---------------------------

thirdA :: [a] -> a
thirdA = head . tail . tail

thirdB :: [a] -> a
thirdB = (!! 2)

thirdC :: [a] -> a
thirdC (_:_:third:_) = third
thirdC other = error ("list of size [" <> (show . length $ other) <> "] is not supported")

-- ---------------------------

-- conditional expression
-- guarded expression
-- pattern matching
safetailA :: [a] ->  [a]
safetailA xs = if null xs then [] else tail xs

safetailB :: [a] ->  [a]
safetailB xs | null xs = []
             | otherwise = tail xs

safetailC :: [a] ->  [a]
safetailC (_:t) = t
safetailC _ = []

------------------------------
-- 1
(||) :: Bool -> Bool -> Bool
(||) True False = True
(||) True True = True
(||) False True = True
(||) False False = False

-- 2
or2 :: Bool -> Bool -> Bool
or2 False False = False
or2 _ _ = True

-- 3
or3 :: Bool -> Bool -> Bool
or3 True _ = True
or3 _ True = True
or3 _ _ = False

-- 4
or4 :: Bool -> Bool -> Bool
or4 a b | not a && not b = False
        | otherwise = True

-------------------------------

andLogical :: Bool -> Bool -> Bool
andLogical a b = if a then (if b then True else False) else False

-------------------------------

orLogical :: Bool -> Bool -> Bool
orLogical a b = if (a) then True else (if (b) then True else False)

-------------------------------

mult :: Int -> Int -> Int -> Int
mult = \a -> (\b -> (\c -> a*b*c))

-------------------------------

-- >>> luhnDouble 9
-- 9

-- >>> luhnDouble 3
-- 6
luhnDouble :: (Ord a, Num a) => a -> a
luhnDouble a | doubled > 9 = result
             | otherwise = doubled
            where
               doubled = a*2
               result = doubled - 9

-- >>> luhn 1 7 8 4
-- True

-- >>> luhn 4 7 8 3
-- False
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = result `mod` 10 == 0
               where
                  result = sum . (++ [b, d]) . map luhnDouble $ [a, c]

