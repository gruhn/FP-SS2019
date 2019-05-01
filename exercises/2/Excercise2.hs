--Excercise 2

-- a)

data List a = Nil | Cons a (List a) deriving Show

instance Eq a => Eq (List a) where
    Nil == Nil                       = True
    (Cons v1 n1) == (Cons v2 n2)     = (v1 == v2) && (n1 == n2)
    _ == _                           = False


-- b)

class Eq a => Mono a where
    binOp :: a -> a -> a 
    one :: a
    pow :: Word -> a -> a
    pow 0 x = one
    pow n x = binOp x (pow (n-1) x)

-- c)

instance Mono Integer where
    binOp a b = a*b
    one = 1
    
instance Eq a => Mono (List a) where
    binOp x Nil = x
    binOp Nil x = x
    binOp (Cons a1 b1) (Cons a2 b2) = Cons a1 (binOp b1 (Cons a2 b2))
    one = Nil

-- d)

multiply :: Mono a => [(Word,a)] -> a
multiply [] = one
multiply ((x,y):xs) = binOp (pow x y) (multiply xs)