
-- 4a)
data Polynomial a = Coeff a Int (Polynomial a) | Null deriving Show

foldPoly :: (a -> Int -> b -> b) -> b -> Polynomial a -> b
foldPoly _ e Null = e
foldPoly f e (Coeff c n p) = f c n (foldPoly f e p)

-- 4b)
degree :: Polynomial Int -> Int
degree p = foldPoly maxDeg minBound p
  where
    maxDeg c n m
      | c == 0 = m
      | m >= n = m
      | otherwise = n
