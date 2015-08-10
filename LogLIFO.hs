import Prelude hiding (tail)

-- | A non-empty LIFO queue with O(log n) indexing.
--
-- Amortized operation for 'cons' is O(1), but in combination with 'tail' it
-- can be O(log n).
--
-- To achieve O(1) it's necessary to add redundancy so that for each nested
-- operation we have one non-nested.
data LogLIFO a = Value a | Zero (LogLIFO (a, a)) | One a (LogLIFO (a, a))
  deriving (Read, Show, Eq, Ord)

singleton :: a -> LogLIFO a
singleton = Value

tail :: LogLIFO a -> (a, Maybe (LogLIFO a))
tail (Value x) = (x, Nothing)
tail (One x xs) = (x, Just $ Zero xs)
tail (Zero xs) = case tail xs of
                    ((x, y), Nothing) -> (x, Just $ singleton y)
                    ((x, y), Just ys) -> (x, Just $ One y ys)

cons :: a -> LogLIFO a -> LogLIFO a
cons x (Zero ys) = One x ys
cons x (One y ys) = Zero (cons (x, y) ys)
cons x (Value y) = Zero (Value (x, y))

(!) :: LogLIFO a -> Int -> Maybe a
Value x ! 0 = Just x
Value _ ! _ = Nothing
One x _  ! 0 = Just x
One _ xs ! n = recIndex xs (n - 1)
Zero xs ! n = recIndex xs n

recIndex :: LogLIFO (a, a) -> Int -> Maybe a
recIndex xs n = case xs ! (n `div` 2) of
                    Just (x, y) | even n     -> Just x
                                | odd n      -> Just y
                    _                        -> Nothing
