module FIFOlazy
    ( empty
    , head
    , tail
    , snoc
    ) where

import Prelude hiding (head, tail)

-- | Invariants:
-- @frontlen >= rearlen@.
-- Front is never empty (if the whole queue is empty, it's just 'Empty').
--
-- Since we rotate only when @rearlen = frontlen + 1@, each list in the
-- middle queue is larger than the preceding one (the first one larger than @front@).
data FIFO a
    = Empty
    | FIFO
        [a]         -- front
        (FIFO [a])  -- middle queue of rotated rears
        Int         -- length of front + middle combined
        [a]         -- rear
        Int         -- rear length
  deriving (Show)

empty :: FIFO a
empty = Empty

head :: FIFO a -> Maybe a
head (FIFO (x:_) _ _ _ _) = Just x
head _ = Nothing

size :: FIFO a -> Int
size (FIFO _ _ fl _ rl) = fl + rl

-- | An internal helper that maintains the required invariant.
fifo :: [a] -> FIFO [a] -> Int -> [a] -> Int -> FIFO a
fifo _ _ 0 _ 0 = Empty
fifo [] q fl r rl | Just (f', q') <- tail q
                    = FIFO f' q' fl r rl
                  | otherwise -- note in this case @rl == 1@
                    = FIFO (reverse r) Empty rl [] 0
fifo f q fl r rl | fl < rl    = FIFO f (snoc q (reverse r)) (fl + rl) [] 0
                 | otherwise  = FIFO f q fl r rl

tail :: FIFO a -> Maybe (a, FIFO a)
tail (FIFO (x:f') q fl r rl)  = Just (x, fifo f' q (fl - 1) r rl)
tail _                        = Nothing

snoc :: FIFO a -> a -> FIFO a
snoc (FIFO f q fl r rl) x = fifo f q fl (x : r) (rl + 1)
snoc Empty              x = fifo [x] Empty 1 [] 0
