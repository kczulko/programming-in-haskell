module Excs5 where

import Data.Char (ord, chr, isLower, isAsciiLower, toLower, toUpper)

-- >>> lowers "absdfF"
-- 5
lowers :: String -> Int
lowers s = length [ x | x <- s, isAsciiLower x ]

-- >>> count 'a' "Abcsda"
-- 1
count :: Char -> String -> Int
count c s = length [x | x <- s, x == c]

-- >>> ord 'a'
-- 97
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- >>> int2let 0
-- 'a'
int2let :: Int -> Char
int2let n = chr $ ord 'a' + n

-- >>> shift 1 'a'
-- 'b'

-- why 26? length ['a'..'z'] == 26
shift :: Int -> Char -> Char
shift n c | isLower c = int2let . (`mod` 26) . (+n) . let2int $ c
          | otherwise = c

-- >>> encode 3 "haskell"
-- "kdvnhoo"
encode :: Int -> String -> String
-- encode n :: map (shift n)
encode n s = [ shift n c | c <- s ]

-- >>> length table
-- 26
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1,
         7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9,
         0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- >>> percent 5 15
-- 33.333336
percent :: Int -> Int -> Float
percent a b = 100 * (fromIntegral a / fromIntegral b)

-- >>> freqs "abcc"
-- [25.0,25.0,50.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
freqs :: String -> [Float]
freqs xs = [ percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(osi - esi)^(2 :: Integer) / esi | (osi, esi) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- >>> positions 'a' "bera gawe"
-- [3,6]
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (i,x') <- zip [0..] xs , x == x']

-- >>> 
-- "kdvnhoo lv ixq"

-- >>> crack $ encode 3 "haskell is funy thing"
-- "haskell is funy thing"
crack :: String -> String
crack xs = encode (-factor) xs
           where
             factor = head (positions (minimum chitab) chitab)
             chitab = [chisqr (rotate n table') table | n <- [0..25]]
             table' = freqs xs



--------------------------------------------------------------------------------
-- Excercises for chapter 5
--------------------------------------------------------------------------------

-- >>> task1
-- 338350
task1 :: Integer
task1 = sum [ x^(2 :: Integer) | x <- [1..100] ]

-- >>> grid 1 2

grid :: Int -> Int -> [(Int,Int)]
grid x y = [(x',y') | x' <- [0..x], y' <- [0..y]]

-- >>> square 2
-- [(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- >>> replicate' 5 'a'
-- "aaaaa"
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- >>> pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
pyths :: Int -> [(Int,Int,Int)]
pyths limit = [ (a,b,c) |
                a <- [1..limit],
                b <- [1..limit],
                c <- [1..limit],
                a^two + b^two == c^two
                        ]
  where two = 2 :: Integer

-- >>> perfects 500
-- [6,28,496]
perfects :: Int -> [Int]
perfects n = [ candidate | candidate <- [1..n], isPerfect candidate ]
  where factors n' = [f | f <- [1..n'], n' `mod` f == 0 ]
        isPerfect n' = n' == (sum . filter (/= n') . factors $ n')

-- >>> scalarproduct [1..3] [4..6]
-- 32
scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [ x*y | (x,y) <- zip xs ys ]

-- >>> products 2 3
-- [(1,1),(2,1),(1,2),(2,2),(1,3),(2,3)]
products :: (Num a, Enum a) => a -> a -> [(a,a)]
products x y = concat [ [(x',y') | x' <- [1..x]] | y' <- [1..y]]

-- >>> find True [(False, 3), (True, 1), (True, 0), (False, 4)]
-- [1,0]
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [b' | (k',b') <- t, k == k']

-- >>> positions2 3 [0, 1, 3, 3, 5, 3]
-- [2,3,5]
positions2 :: Eq a => a -> [a] -> [Int]
positions2 k t = find k (zip t [0..])

-- >>> encode2 (-2) $ encode2 2 "Haskell is Fun"
-- "Haskell is Fun"
encode2 :: Int -> String -> String
encode2 n s = result
  where
        lowercaseInput = map toLower s
        encodeResult = encode n lowercaseInput
        uppsercasePositions = map (not . isLower) s
        modulate cond | cond = toUpper
                      | otherwise = toLower
        result = [modulate isUppercase c | (c, isUppercase) <- zip encodeResult uppsercasePositions ]

