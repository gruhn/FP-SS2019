
-- 1a)
collatz :: Int -> [Int]
collatz n = tail (iterate f n)
  where
    f n | even n = n `div` 2
        | odd  n = 3*n + 1

total_stopping_time :: Int -> Int
total_stopping_time n =
  1 + length (takeWhile (/=1) (collatz n))

-- 1b)

-- QUESTION:
-- So... the function should basically always return True,
-- unless the collatz conjecture does not hold for a certain input.
-- In which case the function should not terminate?
check_collatz :: Int -> Bool
check_collatz n
  = all (<maxBound)
  . map total_stopping_time
  $ [1 .. n]
