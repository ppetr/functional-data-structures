module FIFO
    ( empty
    , head
    , tail
    , snoc
    ) where

import Prelude hiding (head, tail)

-- | Invariant: if @front@ is empty, so is @rear@, which guarantees O(1) time
-- for @head@.
--
-- Credit counting invariant: The number of credits is always equal to
-- @length (read q)@.
data FIFO a = FIFO
    { front :: [a]
    , rear  :: [a]
    }
  deriving (Show)

-- * internal helpers

-- | An internal helper (smart constructor) that maintains the required
-- invariant. If the 'front' becomes empty, replace it with the reversed 'rear'.
--
-- Credit counting: If maintaining the invariant requires reversing 'rear', we
-- discharge all the credits to pay for the operation. At the end, we have no
-- credits and an empty 'rear', maintaining the counting invariant.
-- Otherwise no change.
fifo :: [a] -> [a] -> FIFO a
fifo [] r = FIFO (reverse r) []
fifo f  r = FIFO f r

-- * public functions

empty :: FIFO a
empty = FIFO [] []

head :: FIFO a -> Maybe a
head (FIFO (x:_) _) = Just x  -- no need to check 'rear' thanks to the invariant
head _              = Nothing

tail :: FIFO a -> Maybe (FIFO a)
tail (FIFO (_:f') r)  = Just $ fifo f' r
tail _                = Nothing

-- Appends an element to the queue.
--
-- Credit counting: As we're prolonging 'rear', we add one credit. As it's
-- constant, the amortized cost is still /O(1)/.
-- The internal call to 'fifo' can discharge credits, if a reversal is
-- performed.
snoc :: FIFO a -> a -> FIFO a
snoc (FIFO f r) x = fifo f (x : r)
