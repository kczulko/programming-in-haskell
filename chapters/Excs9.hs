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
-- True
-- >>> isChoice [] [3,2,1,6]
-- True
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (_ : _) [] = False
isChoice (x : xs) ys = case partition (== x) ys of
                         ([], _) -> False
                         _ -> isChoice xs ys


-- answer for 3 - it won't return all the possibile solutions since they are defined through list-comprehension
data Op = Add | Sub | Mul | Div | Exp
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")"

-- >>> split [1..5]
-- [([1],[2,3,4,5]),([1,2],[3,4,5]),([1,2,3],[4,5]),([1,2,3,4],[5])]
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]


combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- >>> exprs [1..2]
-- [App + (Val 1) (Val 2),App - (Val 1) (Val 2),App * (Val 1) (Val 2),App / (Val 1) (Val 2)]
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
            l <- exprs ls,
            r <- exprs rs,
            e <- combine l r]

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && y /= 0 && x `mod` y == 0
valid Exp x y = x /= 0 && x /= 1 && y >= 0

-- >>> apply Exp 2 3
-- 8
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                    y <- eval r,
                    valid o x y]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr,Int)
results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls,rs) <- split ns,
              lx <- results ls,
              ry <- results rs,
              res <- combine' lx ry]
             where
               combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

digits :: [Int]
digits = [1,3,7,10,25,50]

-- >>> totalResults digits
-- 33665406
totalResults :: [Int] -> Int
totalResults xs = length [e | choice <- choices xs,
                          e <- exprs choice]

-- >>> validResults digits
-- 10839369
validResults :: [Int] -> Int
validResults xs = length [r | choice <- choices xs,
                          e <- exprs choice,
                          r <- eval e]

