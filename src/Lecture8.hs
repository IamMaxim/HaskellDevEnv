{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveFunctor #-}

module Lecture8 where


-- class Semigroup a where
--   (<>) :: a -> a -> a
-- 
-- class Semegroup a => Monoid a where
--   mempty :: a
-- 
-- x <> (y <> z) = (x <> y) <> z
-- x <> mempty = x
-- mempty <> x = x
--
--
--
-- 
-- 

-- newtype Sum a = Sum { getSum :: a }
--   deriving (Show)
--
-- instance Num a => Semigroup (Sum a) where
--   Sum x <> Sum y = Sum (x + y)
--
--
-- newtype Max a = Max { getMax :: a }
--   deriving (Show)
--
-- instance Ord a => Semigroup (Max a) where
--   Max x <> Max y = Max (max x y)
--
-- sum :: Num a => [a] -> a
-- sum = getSum . mconcat . map Sum
--
-- maximum' :: Ord a => [a] -> Maybe a
-- maximum' =
--   fmap getMax
--   . (mconcat :: [Maybe (Max a)] -> Maybe (Max a))
--   . map (Just . Max)

newtype Sorted a
  = Sorted { getSorted :: [a] }

mergeSorted
  :: Ord a
  => Sorted a -> Sorted a -> Sorted a
mergeSorted (Sorted xs) (Sorted ys)
  = Sorted (merge xs ys)
  where
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys


instance Ord a => Semigroup (Sorted a) where
  (<>) = mergeSorted

instance Ord a => Monoid (Sorted a) where
  mempty = Sorted []

singleton :: a -> Sorted a
singleton x = Sorted [x]

sorted :: Ord a => [a] -> Sorted a
sorted = mconcat . map singleton

sort :: Ord a => [a] -> [a]
sort = getSorted . sorted


---------------------------------------------------------------------------------


data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) (Tree a)
  deriving (Show, Functor)


balancedTree :: [a] -> tree a
balancedTree = _
  where
    emerge :: [Tree a] -> Tree a
    emerge [] = Empty
    emerge [t] = t
    emerge ts = emerge (pairs ts)

    pairs :: [Tree a] -> [Tree a]
    pairs (l:r:ts)
      = Node l r : pairs ts
    pairs ts = ts

foldTree
  :: Monoid m => Tree m -> m
foldTree Empty = mempty
foldTree (Leaf x) = x
foldTree (Node l r)
  = foldTree l <> foldTree r

-- mergesort :: Ord a => [a] -> [a]
-- mergesort
--   = getSorted
--   . foldTree       -- | This is basically
--   . fmap singleton -- | a map-reduce.
--   . balancedTree

-- Can be rewritten as: ---------------------------------------------------------

foldMapTree
  :: Monoid m
  => (a -> m) -> Tree a -> m
foldMapTree f = foldTree . fmap f

mergesort :: Ord a => [a] -> [a]
mergesort
  = getSorted
  . foldMapTree singleton
  . balancedTree

foldMapList
  :: Monoid m
  => (a -> m) -> [a] -> m
foldMapList f = mconcat . map f

-- class Foldable' t where


---------------------------------------------------------------------------------
