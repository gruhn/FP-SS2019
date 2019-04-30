
-- 1a)
data PriorityQueue a = EmptyQueue |  Push a Int (PriorityQueue a)
  deriving Show

p :: PriorityQueue Int
p = Push 11 1
  $ Push 5 3
  $ Push 5 0
  $ Push 9 (-1)
  $ Push 7 3
  $ Push 8 (-3)
  $ EmptyQueue

-- 1b)
isWaiting :: Eq a => a -> PriorityQueue a -> Bool
isWaiting _ EmptyQueue = False
isWaiting y (Push x _ xs)
  | x == y = True
  | otherwise = isWaiting y xs

-- 1c)
fromList :: [(a, Int)] -> PriorityQueue a
fromList [] = EmptyQueue
fromList ((x, n):xs) = Push x n (fromList xs)

-- 1d)
pop :: PriorityQueue a -> (a, PriorityQueue a)
pop EmptyQueue = undefined
pop (Push x n xs) = (y,ys)
  where
    (Push y _ ys) = maxFirst xs (Push x n EmptyQueue)
    maxFirst :: PriorityQueue a -> PriorityQueue a -> PriorityQueue a
    maxFirst EmptyQueue result = result
    maxFirst (Push x n xs) (Push y m ys)
      | n > m = maxFirst xs (Push x n (Push y m ys))
      | otherwise = maxFirst xs (Push y m (Push x n ys))

-- 1e)
toList :: PriorityQueue a -> [a]
toList EmptyQueue = []
toList xs = y : toList ys
  where
    (y, ys) = pop xs
