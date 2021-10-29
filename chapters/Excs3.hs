module Excs3 where

-- exc 1
one1 :: [Char]
one1 = ['a', 'b', 'c']

one2 :: (Char, Char, Char)
one2 = ('a', 'b', 'c')

one3 :: [(Bool, Char)]
one3 = [(False, '1'), (True, '0')]

one4 :: ([Bool], [Char])
one4 = ([True, False], ['0', '1'])

one5 :: [[a] -> [a]]
one5 = [tail, init, reverse]

-- exc2

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[], [1], [1,2,3]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply fun = fun

-- exc3

second :: [a] -> a
second = head . tail

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = 2*x

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
