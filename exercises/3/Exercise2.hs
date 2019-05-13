
drop_mult :: Int -> [Int] -> [Int]
drop_mult x xs = [ y | y <- xs , y `mod` x /= 0]

dropall :: [Int] -> [Int]
dropall ( x : xs ) = x : dropall ( drop_mult x xs )

primes :: [Int]
primes = dropall [2 ..]

-- 2a)
goldbach :: Int -> [(Int,Int)]
goldbach x =
  [(a, b) | 
    a <- (takeWhile (<x) primes),
    b <- (takeWhile (<x) primes),
    odd a,
    odd b,
    a <= div x 2,
    b > div x 2,
    a+b == x
  ]

-- 2b)
range :: [a] -> Int -> Int -> [a]
range xs m n = [ x | (i, x) <- zip [0 ..] xs, i >= m, i <= n ]
