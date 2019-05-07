--Excercise 1

-- a)

collatz :: Int -> [Int] 
collatz x = iterate (\y -> if mod y 2 == 0 then div y 2 else 3*y+1) x 
    
    
total_stopping_time :: Int -> Int
total_stopping_time 1 = 3
total_stopping_time x = length (takeWhile (\y -> y /= 1) (collatz x))

-- b)

check_collatz :: Int -> Bool
check_collatz 1 = False
check_collatz n = elem 1 (take n (collatz n))


--Excercise 2

-- a)

drop_mult :: Int -> [Int] -> [Int]
drop_mult x xs = [y | y <- xs , mod y x /= 0]

dropall :: [Int]-> [Int]
dropall (x:xs) = x : dropall (drop_mult x xs)

primes :: [Int]
primes = dropall [2 ..]

goldbach :: Int -> [(Int,Int)]
goldbach n = [(x,y)| mod n 2 == 0, x<-takeWhile (<n) primes, y<-filter (>=x) (takeWhile (<n) primes), x+y == n]

-- b) 

range :: [a] -> Int -> Int -> [a]
range xs a b = [xs !! i| i <- [a .. b]] 