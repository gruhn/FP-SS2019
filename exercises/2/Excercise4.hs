--Excercise 4 

-- a)

data Polynomial a = Coeff a Int (Polynomial a)| Null deriving Show

q :: Polynomial Int
q = Coeff 4 3 (Coeff 2 1 (Coeff 5 0 Null))

foldPoly :: (a -> Int -> b -> b) -> b -> Polynomial a -> b
foldPoly f d Null = d
foldPoly f d (Coeff a b c) = f a b (foldPoly f d c)

-- b)

degree :: Polynomial Int -> Int
degree x = foldPoly (\ c n m -> if n > m then n else m) minBound x