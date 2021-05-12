-- | AI for some single-player game

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}


module Lecture8_3 where

import Data.Semigroup

---

second :: (b -> b') -> (a, b) -> (a, b')
second f (a, b) = (a, f b)

---

data Board
data Move


possibleMoves :: Board -> [Move]
possibleMoves = undefined

applyMove :: Move -> Board -> Board
applyMove = undefined



ai :: Board -> Move
ai n
  = fmap extractMove
  . foldMap (fmap Max)
  . fmap estimate
  . propagateRootMoves
  . toOutcomes
  . cutoff n
  . gameTree possibleMoves applyMove

---

data GameTree move board
  = GameTree board [(move, GameTree move board)]
  deriving (Functor, Foldable)

data Outcomes move outcome
  = Outcome outcome
  | ONode [(move, Outcomes move outcome)]
  deriving (Functor, Foldable)

propagateRootMoves
  :: Outcomes move outcome
  -> Outcomes move (Maybe (Arg outcome move))
propagateRootMoves (Outcome x) = Outcome Nothing
propagateRootMoves (ONode ts)
  = ONode (map
           (\(m, t) -> (m, fmap
             (\o -> Just (Arg o m))
             t))
           ts)


toOutcomes :: GameTree m b -> Outcomes m b
toOutcomes (GameTree board []) = Outcome board
toOutcomes (GameTree _ ts) = ONode (map (second toOutcomes) ts)

gameTree
  :: (Board -> [Move])
  -> (Move -> Board -> Board)
  -> Board -> GameTree Move Board
gameTree movesFrom apply board =
  GameTree board subtrees
  where
    subtrees = map tryMove moves
    tryMove move
      = (move, gameTree movesFrom apply
          (apply move board))
    moves = movesFrom board

cutoff :: Int -> GameTree m b -> GameTree m b
cutoff n (GameTree b ts)
  | n <= 0 = GameTree b []
  | otherwise = GameTree b (map (second (cutoff (n - 1))) ts)
