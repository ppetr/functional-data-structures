module FIFOlazy
    ( empty
    , head
    , tail
    , snoc
    ) where

import Prelude hiding (head, tail)

-- | Invariant: @frontlen >= rearlen@
data FIFO a = FIFO
    { front :: [a]
    , frontlen :: Int
    , rear  :: [a]
    , rearlen :: Int
    }
  deriving (Show)

empty :: FIFO a
empty = FIFO [] 0 [] 0

head :: FIFO a -> Maybe a
head (FIFO (x:_) _ _ _) = Just x
head _                  = Nothing

size :: FIFO a -> Int
size (FIFO _ fl _ rl) = fl + rl

-- | An internal helper that maintains the required invariant.
fifo :: [a] -> Int -> [a] -> Int -> FIFO a
fifo f fl r rl | fl < rl    = FIFO (f ++ reverse r) (fl + rl) [] 0
               | otherwise  = FIFO f fl r rl

tail :: FIFO a -> Maybe (FIFO a)
tail (FIFO (_:f') fl r rl)  = Just $ fifo f' (fl - 1) r rl
tail _                      = Nothing

snoc :: FIFO a -> a -> FIFO a
snoc (FIFO f fl r rl) x = fifo f fl (x : r) (rl + 1)
