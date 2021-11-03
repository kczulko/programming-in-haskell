module Excs7 where

import Data.Char
import Excs4 (luhnDouble)

-- >>> showEquality [1,2,3,4,5] (>2) (*2)
-- True
showEquality :: (Eq b) => [a] -> (a -> Bool) -> (a -> b) -> Bool
showEquality xs p f = [f x | x <- xs, p x] == (map f . filter p $ xs)

-- >>> all' even [1..10] 
-- False
-- >>> all' even [2,4,6,8,10] 
-- True
-- >>> all' even [] 
-- False
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = case xs of
  [] -> False
  (_ : _) -> foldl (\b a -> b && f a) True xs

-- >>> any even [1..4]
-- True
-- >>> any (>5) [1..4]
-- False
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl (\b a -> b || f a) False

-- >>> takeWhile' (<5) [1..]
-- [1,2,3,4]
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

-- >>> take 10 $ dropWhile' (<5) [1..]
-- [5,6,7,8,9,10,11,12,13,14]
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f ls @ (x:xs) | f x = dropWhile' f xs
                         | otherwise = ls

-- >>> mapInTermsOfFoldr (+1) [1..5]
-- [2,3,4,5,6]
mapInTermsOfFoldr :: (a -> b) -> [a] -> [b]
mapInTermsOfFoldr = map

-- >>> filterInTermsOfFoldr (<5) [1..20]
-- [1,2,3,4]
filterInTermsOfFoldr :: (a -> Bool) -> [a] -> [a]
filterInTermsOfFoldr p = foldr concatOnPredicate []
  where concatOnPredicate x xs | p x = x : xs
                               | otherwise = xs
-- >>> dec2int [1,2,3,4]
-- 1234
dec2int :: [Int] -> Int
dec2int = foldl (\b a -> a + 10*b) 0


addTest :: (Int,Int) -> Int
addTest (a,b) = a + b
-- >>> curry' addTest 4 5
-- 9
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f a b = f (a,b)

-- >>> uncurry' (+) (4,5)
-- 9
-- >>> uncurry' (curry' addTest) $ (4,5)
-- 9
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a,b) = f a b

unfold' :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold' p h t x | p x = []
                | otherwise = h x : unfold' p h t (t x)

type Bit = Int

-- >>> int2bin 8
-- [0,0,0,1]
int2bin :: Int -> [Bit]
int2bin = unfold' (== 0) (`mod` 2) (`div` 2)

chop :: Int -> [Bit] -> [[Bit]]
chop n = unfold' null (take n) (drop n)

-- >>> chop8 [1,0,0,0,1,0,0,0,1,0,0,1,0,0,1]
-- [[1,0,0,0,1,0,0,0],[1,0,0,1,0,0,1]]
chop8 :: [Bit] -> [[Bit]]
chop8 = chop 8

-- >>> mapInTermsOfUnfold (+1) [1..5]
-- [2,3,4,5,6]
mapInTermsOfUnfold :: (a -> b) -> [a] -> [b]
mapInTermsOfUnfold f = unfold' null (f. head) tail

-- >>> take 5 (iterateInTermsOfUnfold (+1) 1)
-- [2,3,4,5,6]
iterateInTermsOfUnfold :: (a -> a) -> a -> [a]
iterateInTermsOfUnfold f = unfold' (const False) f f

--------------------------------------------------------------------------------

-- >>> bin2int [0,0,0,1]
-- 8
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

make :: Int -> [Bit] -> [Bit]
make n bits = take n (bits ++ repeat 0)

make8 :: [Bit] -> [Bit]
make8 = make 8

obtainParityBit :: [Bit] -> Int
obtainParityBit xs | odd . sum . filter (==1) $ xs = 1
                   | otherwise = 0

-- >>> encode "Haskell is fun!"
-- [0,0,0,1,0,0,1,0,1,0,0,0,0,1,1,0,1,1,0,0,1,1,1,0,1,1,0,1,0,1,1,0,1,0,1,0,0,1,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,1,1,0,1,1,0,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,1,0,1,0,1,0,1,1,1,0,0,1,1,1,0,1,1,0,1,0,0,0,0,1,0,0]
encode :: String -> [Bit]
encode = concatMap $ addParity . make 8 . int2bin . ord
  where addParity xs = obtainParityBit xs : xs

-- >>> decode (encode "Haskell is fun!")
-- "Haskell is fun!"
decode :: [Bit] -> String
decode = map (chr . bin2int . validateAndDropParityBit) . chop 9
  where validateAndDropParityBit [] = error "Received empty array"
        validateAndDropParityBit xs@(h:t) | obtainParityBit t == h = t
                                          | otherwise = error ("Transmission error detected " <> show xs)
-- >>> transmit id "Haskell is fun!!!"
-- "Haskell is fun!!!"
-- >>> transmit firstBitFailureChannel "Haskell is fun!!!"
-- Transmission error detected[1,0,0,0,1,0,0,1,0]

transmit :: ([Bit] -> [Bit]) -> String -> String
transmit channel = decode . channel . encode

firstBitFailureChannel :: [Bit] -> [Bit]
firstBitFailureChannel [] = []
firstBitFailureChannel bits@(h:t) | h == 0 = 1 : t
                                  | otherwise = bits


items :: [String -> [Bit]]
items = encode : items

-- >>> altMap (+10) (+100) [0..6]
-- [10,101,12,103,14,105,16]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = [fg x | (fg,x) <- zip functions xs]
  where functions = f : g : functions

-- >>> luhn [1,7,8,4]
-- True
-- >>> luhn [4,7,8,3]
-- False
luhn :: [Int] -> Bool
luhn = (== 0) . (`mod` 10) . sum . altMap luhnDouble id
