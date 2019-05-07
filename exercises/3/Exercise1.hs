
-- 1a)

-- QUESTION:
-- Should the first entry of the result list be the case k=0 ?
-- Not sure if the exercise implies that. That's means however,
--    collatz 1 = [1, 4, 2, 1, ...]
-- So the smallest k in this case would actually be k=0.
collatz :: Int -> [Int]
collatz n = iterate f (f n)
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
