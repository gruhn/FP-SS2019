--Excercice 3

-- a) 

neq :: Eq a => a -> [a] -> [a]
neq x xs = x : filter (/=x) xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates xs = foldr neq xs xs


-- b)

differentDigits :: Int -> Int
differentDigits x = foldr (+) 0 (map (\x -> 1) (removeDuplicates (show x)))