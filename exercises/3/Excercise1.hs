--Excercise 1

-- a)

collatz :: Int -> [Int] 
collatz x = iterate (\y -> if mod y 2 == 0 then div y 2 else 3*y+1) x 
    
    
total_stopping_time :: Int -> Int
total_stopping_time 1 = 3
total_stopping_time x = length (takeWhile (/= 1) (collatz x))

-- b)

check_collatz :: Int -> Bool
check_collatz 1 = False
check_collatz n = elem 1 (take n (collatz n))