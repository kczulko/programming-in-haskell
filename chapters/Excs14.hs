module Excs14 where

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
-- mempty = (mempty, mempty)
-- (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- instance Monoid b => Monoid (a -> b) where
  -- mempty = const mempty
  -- mappend f g = \a -> f a `mappend` fg

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Foldable Tree where
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse g (Node l a r) = Node <$> traverse g l <*> g a <*> traverse g r

tree :: Tree Integer
tree = Node (Node Leaf 4 Leaf) 2 (Node Leaf 3 Leaf)

treeMaybe :: Tree (Maybe Integer)
treeMaybe = Node (Node Leaf (Just 4) Leaf) Nothing (Node Leaf (Just 3) Leaf)

-- >>> traverse (\a -> if (a < 5) then (Just a) else Nothing ) tree
-- Just (Node (Node Leaf 4 Leaf) 2 (Node Leaf 3 Leaf))

-- >>> sequence treeMaybe
-- Nothing

-- >>> filterF even tree
-- [4,2]

-- >>> filterF even [1..7]
-- [2,4,6]
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap decide
            where decide a | f a = [a]
                           | otherwise = []
