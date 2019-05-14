
-- 1a)
collatz :: Int -> [Int]
collatz n = iterate f n
  where
    f n | even n = n `div` 2
        | odd  n = 3*n + 1

total_stopping_time :: Int -> Int
total_stopping_time n =
  length (takeWhile (/=1) (collatz n))

-- 1b)
check_collatz :: Int -> Bool
check_collatz n
  = all (<maxBound)
  . map total_stopping_time
  $ [1 .. n]
