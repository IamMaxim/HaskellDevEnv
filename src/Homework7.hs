{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module Homework7 where

import Data.List (sortBy)

-- | Stores the intermediate information about mean computation.
data Mean a = Mean Int a
  deriving (Show)

instance Fractional a => Semigroup (Mean a) where
  (Mean c1 v1) <> (Mean c2 v2) = Mean (c1 + c2) (v1 + delta * fromIntegral c2 / fromIntegral (c1 + c2))
    where
      delta = v2 - v1

instance Fractional a => Monoid (Mean a) where
  mempty = Mean 0 0

-- | Computes mean of the given list.
mean :: Fractional a => [a] -> Mean a
mean = mconcat . map (Mean 1)

--------------------------------------------------------------------------------

-- | Intermediate results for
-- count, mean and variance.
data Variance a = Variance Int a a
  deriving (Show)

instance Fractional a => Semigroup (Variance a) where
  (Variance c1 m1 v1) <> (Variance c2 m2 v2) =
    Variance (c1 + c2) newMean newVar
    where
      fi = fromIntegral
      delta = m2 - m1
      Mean _ newMean = Mean c1 m1 <> Mean c2 m2
      newVar =
        ( v1 * fi (c1 - 1)
            + v2 * fi (c2 - 1)
            + delta * delta * fi c1 * fi c2 / fi (c1 + c2)
        )
          / fi (c1 + c2 - 1)

instance Fractional a => Monoid (Variance a) where
  mempty = Variance 0 0 0

-- | Compute count, mean and variance
-- for a list of values.
variance :: Fractional a => [a] -> Variance a
variance = mconcat . map (\a -> Variance 1 a 0)

--------------------------------------------------------------------------------

-- | A (closed) interval for values of type t.
--
-- Values of this type should always be valid:
--
-- from <= to
data Interval t = Interval
  { -- | Start of an interval.
    from :: t,
    -- | End of an interval.
    to :: t
  }
  deriving (Show)

-- | A set of non-overlapping intervals.
--
-- The invariants for this representation are:
--
-- * the intervals are sorted according to from;
-- * the intervals do not overlap (even at a single point).
newtype IntervalSet t = IntervalSet [Interval t]
  deriving (Show)

-- | Convert an interval set into a list of pairs -- representing each interval.
fromIntervalSet :: IntervalSet t -> [(t, t)]
fromIntervalSet (IntervalSet list) = map (\(Interval f t) -> (f, t)) list

-- | Unsafely construct an interval set from a list of pairs.
-- Input list should be ordered by the first component
-- and intervals, represented by (t, t), should not overlap.
unsafeIntervalSet :: [(t, t)] -> IntervalSet t
unsafeIntervalSet list = IntervalSet (map (uncurry Interval) list)

-- | An invariant-preserving union of two interval sets.
union :: Ord t => IntervalSet t -> IntervalSet t -> IntervalSet t
union (IntervalSet ints1) (IntervalSet ints2) =
  IntervalSet (removeOverlaps (merge ints1 ints2))

-- | Merges two interval sets preserving the sorted order.
-- Assumes that both of input lists are sorted.
merge :: Ord t => [Interval t] -> [Interval t] -> [Interval t]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) =
  if from x < from y
    then x : merge xs (y : ys)
    else y : merge (x : xs) ys

-- | Merges all neighboring intervals that overlap.
-- Assumes that input list is already sorted.
removeOverlaps :: Ord t => [Interval t] -> [Interval t]
removeOverlaps [] = []
removeOverlaps [x] = [x]
removeOverlaps (t1 : t2 : ts) =
  if to t1 >= from t2
    then
      if to t1 >= to t2
        then Interval (from t1) (to t1) : removeOverlaps ts
        else Interval (from t1) (to t2) : removeOverlaps ts
    else t1 : removeOverlaps (t2 : ts)

-- | Construct an interval set from an arbitrary list of intervals.
intervalSet :: Ord t => Ord t => [(t, t)] -> IntervalSet t
intervalSet =
  unsafeIntervalSet
    . map (\(Interval t1 t2) -> (t1, t2))
    . removeOverlaps
    . map (uncurry Interval)
    . sortBy sortFun
    . map (\(t1, t2) -> (min t1 t2, max t1 t2))
  where
    sortFun :: Ord t => (t, t) -> (t, t) -> Ordering
    sortFun (l1, _) (l2, _) =
      if l1 < l2
        then LT
        else GT
