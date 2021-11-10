{-# LANGUAGE ScopedTypeVariables #-}
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

-- >>> occurs 5 (Leaf 5)
-- True
occurs :: Ord a => a -> Tree a -> Bool
occurs a (Leaf a') = a == a'
occurs a (Node left a' right) = case compare a a' of
                                  EQ -> True
                                  LT -> occurs a left
                                  GT -> occurs a right

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left _ right) = abs (countLeaves left - countLeaves right) <= 1
                               && balanced left && balanced right
  where
    countLeaves :: Tree a -> Int
    countLeaves (Leaf _) = 1
    countLeaves (Node l _ r) = countLeaves l + countLeaves r

data BinTree a = BinLeaf | BinNode (BinTree a) a (BinTree a) deriving Show

-- >>> balance [1..5]
-- BinNode (BinNode (BinNode BinLeaf 1 BinLeaf) 2 BinLeaf) 3 (BinNode (BinNode BinLeaf 4 BinLeaf) 5 BinLeaf)
-- >>> balance [1..4]
-- BinNode (BinNode (BinNode BinLeaf 1 BinLeaf) 2 BinLeaf) 3 (BinNode BinLeaf 4 BinLeaf)
-- >>> balance []
-- BinLeaf
-- >>> balance [1]
-- BinNode BinLeaf 1 BinLeaf
balance :: [a] -> BinTree a
balance [] = BinLeaf
balance [a] = BinNode BinLeaf a BinLeaf
balance xs@(_:_) = case splitAt (length xs `div` 2) xs of
                     (ls,r:rs) -> BinNode (balance ls) r (balance rs)
                     (l:ls,[]) -> BinNode (balance ls) l BinLeaf

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde vf af expr = case expr of
                     Val i -> vf i
                     Add e1 e2 -> af (folde vf af e1) (folde vf af e2)

-- >>> eval $ Add (Val 3) (Val 5)
-- 8
-- >>> size $ Add (Val 3) (Val 5)
-- 2
evale :: Expr -> Int
evale = folde id (+)

size :: Expr -> Int
size = folde (const (1 :: Int)) (+)

-- instance Eq a => Eq (Maybe a) where
--   Just a1 == Just a2 = a1 == a2
--   Nothing == Nothing = True
--   _ == _ = False

-- instance Eq a => Eq [a] where
--   [] == [] = True
--   (l:ls) == (r:rs) = l == r && ls == rs
--   _ == _ = False

data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Imply Prop Prop
  | Leq Prop Prop

type Assoc a b = [(a,b)]

type Subst = Assoc Char Bool

find' :: Eq a => a -> Assoc a b -> b
find' _ [] = error "Elem not found"
find' a ((a',b):xs) | a == a' = b
                   | otherwise = find' a xs

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var c) = find' c s
eval s (Not prop) = not $ eval s prop
eval s (And prop1 prop2) = eval s prop1 && eval s prop2
eval s (Or prop1 prop2) = eval s prop1 || eval s prop2
eval s (Imply prop1 prop2) = eval s prop1 <= eval s prop2
eval s (Leq prop1 prop2) = eval s prop1 == eval s prop2

vars :: Prop -> String
vars (Const _) = []
vars (Var c) = [c]
vars (Not prop) = vars prop
vars (And prop1 prop2) = vars prop1 ++ vars prop2
vars (Or prop1 prop2) = vars prop1 ++ vars prop2
vars (Imply prop1 prop2) = vars prop1 ++ vars prop2
vars (Leq prop1 prop2) = vars prop1 ++ vars prop2

-- >>> bools 2
-- [[True,True],[True,False],[False,True],[False,False]]
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (True:) bss ++ map (False:) bss
  where bss = bools $ n-1

-- >>> substs $ And (Or (Var 'C') (Var 'D')) (Not (Var 'A'))
-- [
--   [('C',True),('D',True),('A',True)],
--   [('C',True),('D',True),('A',False)],
--   [('C',True),('D',False),('A',True)],
--   [('C',True),('D',False),('A',False)],
--   [('C',False),('D',True),('A',True)],
--   [('C',False),('D',True),('A',False)],
--   [('C',False),('D',False),('A',True)],
--   [('C',False),('D',False),('A',False)]
--]
substs :: Prop -> [Subst]
substs p = map (vs `zip`) $ bools len
  where len = length vs
        vs = rmdups $ vars p
        rmdups :: Ord a => [a] -> [a]
        rmdups [] = []
        rmdups (x:xs) = x : rmdups (filter (x /=) xs)

isTaut :: Prop -> Bool
isTaut p = and [ eval s p | s <- substs p ]

--------------------------------------------------------------------------------

data Expr1 = Val1 Int | Add1 Expr1 Expr1 | Mul1 Expr1 Expr1

value :: Expr1 -> Int
value (Val1 n) = n
value (Add1 p q) = value p + value q
value (Mul1 p q) = value p * value q

data Op = EVALADD Expr1 | EVALMUL Expr1 | ADD Int | MUL Int
type Cont = [Op]

eval' :: Expr1 -> Cont -> Int
eval' (Val1 n) c = exec c n
eval' (Add1 p q) c = eval' p (EVALADD q:c)
eval' (Mul1 p q) c = eval' p (EVALMUL q:c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALADD y:c) n = eval' y (ADD n:c)
exec (EVALMUL y:c) n = eval' y (MUL n:c)
exec (ADD n:c) m = exec c (n+m)
exec (MUL n:c) m = exec c (n*m)

-- >>> value' $ Add1 (Add1 (Val1 2) (Val1 3)) (Mul1 (Val1 4) (Val1 2))
-- 13
value' :: Expr1 -> Int
value' e = eval' e []
