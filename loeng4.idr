{-
Create from list a list of primes
-}
sieve : List Int -> List Int
sieve [] = []
sieve (x :: xs) = x :: sieve (filter (\ y => y `mod` x /= 0) xs)
-- sieve (x :: xs) = x :: sieve [ y | y <- xs, y `mod` x /= 0 ]

{-
Find primes til number n
-}
primesTill : Int -> List Int
primesTill n = sieve [2..n]

{-
Quick sort
-}
quick : (a -> a -> Bool) -> List a -> List a
quick leq [] = []
quick leq (x :: []) = [x]
quick leq (x :: xs) = 
    quick leq [y | y <- xs, y `leq` x] ++
    [x] ++
    quick leq [y <- ]


