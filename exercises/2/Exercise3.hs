
-- 3a)
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates xs = foldr dropAll xs xs
  where
    dropAll x ys = x : filter (/=x) ys

-- 3b)
differentDigits :: Int -> Int
differentDigits number = foldr count 0 uniqueDigits
  where
    uniqueDigits = removeDuplicates (show number)
    count _ n = n+1
