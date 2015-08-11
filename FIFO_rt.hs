module FIFOrt where

import Prelude hiding (head, tail)

-- | Invariant: @|eval| = |front| - |rear|@.
-- Both 'tail' and 'snoc' are worst-case _O(1)_.
data FIFO a = FIFO
    { front :: [a]
    , eval :: [a]
    , rear  :: [a]
    }
  deriving (Show)

rotate :: [a] -> [a] -> [a]
rotate xs ys = rotate' xs ys []
  where
    rotate' :: [a] -> [a] -> [a] -> [a]
    rotate' (x:xs) (y:ys) as = let as' = y : as
                                in as' `seq` (x : rotate' xs ys as')
    rotate' [] [y] as        = y : as

empty :: FIFO a
empty = FIFO [] [] []

fifo :: [a] -> [a] -> [a] -> FIFO a
fifo f [] r = let f' = rotate f r
               in FIFO f' f' []
fifo f (_:es) r = FIFO f es r

snoc :: FIFO a -> a -> FIFO a
snoc (FIFO f es r) x = fifo f es (x : r)

tail :: FIFO a -> Maybe (a, FIFO a)
tail (FIFO [] _ _) = Nothing
tail (FIFO (x:f') es r) = Just (x, fifo f' es r)
