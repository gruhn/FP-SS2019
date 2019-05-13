--Excercise 1

-- a)

collatz :: Int -> [Int] 
collatz x = iterate (\y -> if even y then div y 2 else (3*y+1)) x
    
    
total_stopping_time :: Int -> Int
total_stopping_time 1 = 3
total_stopping_time x = length (takeWhile (/= 1) (collatz x))

-- b)

check_collatz :: Int -> Bool
check_collatz 1 = True
check_collatz n = (total_stopping_time n)<maxBound && check_collatz (n-1)