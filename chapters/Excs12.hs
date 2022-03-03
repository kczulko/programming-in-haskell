{-# LANGUAGE TupleSections #-}
module Excs12 where

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap fab (Node l a r) = Node (fmap fab l) (fab a) (fmap fab r)

-- >>> check ((Node (Node Leaf 2 Leaf) 1 Leaf) :: Tree Int)
-- Node (Node Leaf 3 Leaf) 2 Leaf
checkTreeFunctor :: Tree Int -> Tree Int
checkTreeFunctor = fmap (+1)

--instance Functor ((->) a) where
--  -- fmap (c -> d) (a -> ) (b -> )
--  fmap f fa = f . fa

-- instance Applicative ((->) r) where
--  -- pure :: r -> f r
--  pure = const
--  -- (<*>) :: f (r -> (a -> b)) -> f (r -> a) -> f (r -> b)
--  fab <*> fa = \r -> fab r (fa r)

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a->b) -> ZipList a -> ZipList b
  fmap f (Z xs)= Z [f x | x <- xs]

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (repeat x)

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  Z ab <*> Z a = Z [f a' | (a',f) <- zip a ab]


-- >>> w2 (take 3 $ repeat (+1)) [1,2,3]
-- [2,3,4]
w2 :: [a->b] -> [a] -> [b]
w2 ab a = [f a' | (f,a') <- zip ab a]

-- >>> check2
-- Z [2,3,4,5,6]
check2 :: ZipList Integer
check2 = (+1) <$> Z [1..5]

-- applicative laws:
-- 1. pure id <*> x == x
-- id :: (a -> a)
-- x :: f a

-- 2. pure (g x) = pure g <*> pure x
-- g :: a -> b
-- x :: a

-- 3. x <*> pure y = pure (\g -> g y ) <*> x
-- x :: f (a -> b)
-- y :: a
-- g :: (a -> b) -> b

-- 4. x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- x :: f (a -> b -> c)
-- y :: f (a -> b)
-- z :: f b

-- ((->) r) => r ->
-- instance Monad ((->) r) where
--   return = const
--  
--   -- (>>=) :: (a -> (r -> b)) -> (r -> a) -> (r -> b)
--   (>>=) afb fa = \r -> afb (fa r) r

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
  -- fmap fab fa
  fmap f (Var a) = Var (f a)
  fmap _ (Val i) = Val i
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
  pure a = Var a
  -- <*> f expr
  Var fab <*> expr = fmap fab expr
  Add l r <*> expr = Add (l <*> expr) (r <*> expr)
  Val i <*> _ = Val i

instance Monad Expr where
  return = pure

  (Var a) >>= faeb = faeb a
  (Add l r) >>= faeb  = Add (l >>= faeb) (r >>= faeb)
  (Val i) >>= _ = Val i

type State = Int
newtype ST a = S (State -> (a,State))

run :: ST a -> State -> (a,State)
run (S f) = f

instance Functor ST where
  fmap fab fa = do a <- fa
                   return (fab a)

instance Applicative ST where
  pure a = S (a,)
  l <*> r = do f <- l
               a <- r
               return (f a)

instance Monad ST where
  return a = S (a,)
  state >>= f = S (\s -> let (a,s') = run state s
                         in run (f a) s')
