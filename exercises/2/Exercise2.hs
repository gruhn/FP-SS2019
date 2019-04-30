
-- 2a)
data List a = Nil | Cons a (List a)
  deriving Show

instance Eq a => Eq (List a) where
  (==) Nil Nil = True
  (==) (Cons x xs) (Cons y ys)
    | x /= y = False
    | otherwise = xs == ys
  (==) _ _ = False

-- 2b)
class Eq a => Mono a where
  binOp :: a -> a -> a
  one :: a
  pow :: Word -> a -> a
  pow 0 _ = one
  pow n x = binOp x (pow (n-1) x)

-- 2c)
instance Mono Integer where
  binOp x y = x * y
  one = 1

instance Eq a => Mono (List a) where
  binOp (Cons x xs) ys = Cons x (binOp xs ys)
  binOp Nil ys = ys
  one = Nil

-- 2d)
multiply :: Mono a => [(Word, a)] -> a
multiply [] = one
multiply ((n,x):xs) = binOp (pow n x) (multiply xs)
