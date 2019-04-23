-- Exercise 1
-- i)
xor :: Bool -> Bool -> Int
xor x y | x == y    = 0
        | otherwise = 1

-- ii)
selectiveSum :: [Int] -> [Bool] -> Int
selectiveSum [] []          = 0
selectiveSum [] (x:xs)      = 0
selectiveSum (x:xs) []      = 0
selectiveSum (x:xs) (y:ys)  = if y then x + rest else rest
        where rest = selectiveSum xs ys

-- iii)
intoInt :: [Bool] -> (Bool->Int) -> [Int]
intoInt [] _     = []
intoInt (x:xs) f = f x : intoInt xs f

-- Exercise 3
-- a)
myrem :: Int -> Int -> Int
myrem x y
  | y == 0 = x
  | y < 0 = myrem x (-y)
  | x < 0 = -(myrem (-x) y)
  | x > y = myrem (x-y) y
  | otherwise = x-y

-- b)
count :: Int -> [Int] -> Int
count y [] = 0
count y (x:xs)
  | y == x = 1 + count y xs
  | otherwise = count y xs

-- c)
simplify :: [Int] -> [(Int, Int)]
simplify [] = []
simplify (x:xs) =
  (x, 1 + count x xs) : simplify (without x xs)
  where
    without x [] = []
    without x (y:ys)
      | x == y = without x ys
      | otherwise = y : without x ys


-- d)
multUnion :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
multUnion [] ys = ys
multUnion xs [] = xs
multUnion ((x,n):xs) ((y,m):ys)
  | x < y = (x,n) : multUnion xs ((y,m):ys)
  | x > y = (y,m) : multUnion ((x,n):xs) ys
  | otherwise = (x,n+m) : multUnion xs ys

-- Exercise 4
(^^^) :: [Int] -> [Int] -> Int
(^^^) [] [] = 1
(^^^) (x:xs) (y:ys) =
  (pow x y) * (xs ^^^ ys)
  where
    pow :: Int -> Int -> Int
    pow x y
      | y <= 0 = 1
      | otherwise = x * (pow x (y-1))
