--Excercise 2

-- a)

drop_mult :: Int -> [Int] -> [Int]
drop_mult x xs = [y | y <- xs , mod y x /= 0]

dropall :: [Int]-> [Int]
dropall (x:xs) = x : dropall (drop_mult x xs)

primes :: [Int]
primes = dropall [2 ..]

goldbach :: Int -> [(Int,Int)]
goldbach n = [(x,y)| even n, x<-takeWhile (<n) primes, y<-filter (>=x) (takeWhile (<n) primes), odd x, odd y, x+y == n]

-- b) 

range :: [a] -> Int -> Int -> [a]
range xs a b = [xs !! i| i <- [a .. b]] 