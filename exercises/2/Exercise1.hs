-- Excercise 1

-- a)

data PriorityQueue a = Push a Int (PriorityQueue a) | EmptyQueue deriving Show

p :: PriorityQueue Int
p = Push 11 1 (Push 5 3 (Push 5 0 (Push 9 (-1) (Push 7 3 (Push 8 (-3) EmptyQueue)))))

-- b)

isWaiting :: Eq a => a -> PriorityQueue a -> Bool
isWaiting _ EmptyQueue               = False
isWaiting x (Push v _ n) | x == v    = True
                         | otherwise = isWaiting x n

-- c)

fromList :: [(a,Int)] -> PriorityQueue a
fromList []           = EmptyQueue
fromList ((x,p):xs)   = Push x p (fromList xs)

-- d)

-- auxiliary functions
delete :: PriorityQueue a -> Int -> PriorityQueue a
delete EmptyQueue _   = EmptyQueue
delete (Push v p n) x = if x==p then n else (Push v p (delete n x))

findElement :: PriorityQueue a -> Int -> a
findElement (Push v p n) x = if x==p then v else findElement n x

highestPriority :: PriorityQueue a -> Int
highestPriority EmptyQueue   = minBound
highestPriority (Push v p n) = max p (highestPriority n)

--main function
pop :: PriorityQueue a -> (a,PriorityQueue a)
pop x = (findElement x h, delete x h)
    where h = highestPriority x

-- Since the PriorityQueue given to the function 'pop' is nonempty both the main function 'pop' and the auxiliary function 'findElement' ignore that case.

-- e)

toList :: PriorityQueue a -> [a]
toList EmptyQueue = []
toList q = x : toList y
   where (x,y) = pop q
