module FIFO
    ( empty
    , head
    , tail
    , snoc
    ) where

import Prelude hiding (head, tail)

-- | Invariant: if @front@ is empty, so is @rear@, which guarantees O(1) time
-- for @head@.
data FIFO a = FIFO
    { front :: [a]
    , rear  :: [a]
    }
  deriving (Show)

empty :: FIFO a
empty = FIFO [] []

head :: FIFO a -> Maybe a
head (FIFO (x:_) _) = Just x
head _              = Nothing

-- | An internal helper that maintains the required invariant.
fifo :: [a] -> [a] -> FIFO a
fifo [] r = FIFO (reverse r) []
fifo f  r = FIFO f r

tail :: FIFO a -> Maybe (FIFO a)
tail (FIFO (_:f') r)  = Just $ fifo f' r
tail _                = Nothing

snoc :: FIFO a -> a -> FIFO a
snoc (FIFO f r) x = fifo f (x : r)
