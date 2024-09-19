module K1

{-
Kirjutage funktsioon sumInt, 
mis saades sisendiks naturaalarvu x tagastab x-st väiksemate 
või võrdsete arvude summa.
-} 

sumInt : Int -> Int
sumInt n = if n == 0 then 0 else sumInt (n-1) + n

{-
Kirjutage funktsioon fib, 
mis leiab n-da Fibonacci arvu. 
-}

fib : Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

{-
Kirjutage jäägiga jagamise 
funktsioon modulo.
-}

modulo : Int -> Int -> Int
modulo x y = f x
  where
    f : Int -> Int
    f n = if n < y then n else f (n - y)

{-
Kirjutage funktsioon syt, 
mis leiab suurima ühisteguri läbi Eukleidese algoritmi.
-}

syt : Int -> Int -> Int
syt x y = if y == 0 then x else syt y (modulo x y)

{-
Kirjutaga Hanoi funktsioon.
-}

hanoi : Int -> Int
hanoi 1 = 1
hanoi n = 2 * hanoi (n-1) + 1

{-
Kirjutada Ackermanni funktsioon.
-}

ack : Int -> Int -> Int
ack 0 n = n + 1
ack m n = if m > 0 && n == 0 then ack (m - 1) 1 else ack (m - 1) (ack m (n - 1))

{-
Kasutades rekursiooni, implementeeri naiivne astendamise funktsioon
-}

aste : Int -> Int -> Int
aste x 0 = 1
aste x n = x * aste x (n - 1)

{-
Implementeeri kiire astendamise funktsioon
-}

qaste : Int -> Int -> Int
qaste x 0 = 1
qaste x n = if n `mod` 2 == 0 then y * y else x * y * y
    where y = qaste x (n `div` 2)

{-
Implementeeri naiivne jagamise funktsioon.
-}

ndiv : Int -> Int -> Int
ndiv x y = f x 0
    where
        f : Int -> Int -> Int
        f n z = if n < y then z else f (n - y) (z + 1)

{-
Kirjutage liitmise funktsioon add, mis kasutab ainult 
oma argumente ning funktsioone inc ja korda.
-}

{-
Kirjutage korrutamise funktsioon mul, mis kasutab ainult oma argumente, 
konstanti 0 ning funktsioone add ja korda.
-}

korda : Int -> (Int -> Int) -> Int -> Int
korda 0 f x = x
korda n f x = f (korda (n - 1) f x)
 
inc : Int -> Int
inc x = x + 1

add : Int -> Int -> Int
add x y = korda y inc x

mul : Int -> Int -> Int
mul x y = korda y (add x) 0