{-
Leia termidest vabad muutujad!
-}

-- FV(λf. (λg. g x) (λx. f x)) 
-- FV((λx. (λg. g x)) x)
-- FV(λg. (λf. f x) (λx. g x))
-- FV((λy. y) (λf x. y x) (λx. g x))
-- FV(z λh x. (λf. h f) x)

{-
Leia substitutsiooni tulemus!
-}

-- (λf. f y(λx.x))[y→λx y. f x]
-- (λx. f (x x))(λx. f (x x))[f→λx y. y]
-- (λx g y.x g y)[g→x g y]

{-
Kasutades listikomprehensiooni, 
leia kõik arvud mis on väiksemad kui 1000 ja jaguvad seitsmega. 
Mitu neid on?
-}

mod7 : List Int
mod7 = [ x | x <- [0..1000], x `mod` 7 == 0]

{-
Kasutades listikomprehensiooni, 
kirjuta funktsioon mis loendab etteantud sõnes 
esimese parameetrina antud tähe leidumiste arvu. 
Vihje: funktsioon unpack teisendab sõne tähtede listiks.
-}

count : Char -> String -> Nat
count c s = length [ l | l <- unpack s, l == c]

{-
Kasutades listikomprehensiooni, 
kirjuta funktsioon mis teisendab listide 
listid listideks hoides alles kõik elemendid samas järjekorras.
-}

concat' : List (List a) -> List a
concat' xss = [ x | xs <- xss, x <- xs]

{-
Kasutades listikomprehensiooni, 
kirjuta funktsioon mis leiab parameetrina antud 
aru n täisarvulised faktorid s.t. arvud, 
millega n-i jagades ei teki jääki.
-}

factors : Int -> List Int
factors n = [ x | x <- [1..n], n `mod` x == 0]

{-
Kasutades listikomprehensiooni, 
kirjuta funktsioon mis leiab parameetrina antud 
arvust n kõik väiksemad algarvud.
-}

isPrime : Int -> Bool
isPrime n = factors n == [1,n]

primes : Int -> List Int
primes n = [ x | x <- [1..n], isPrime x]

{-
Kirjuta lihtrekursiivne funktsioon, 
mis teisendab listide paarid paaride listiks.
-}

zip' : List a -> List b -> List (a,b)
zip' [] y = []
zip' x [] = []
zip' (x::xs) (y::ys) = (x, y) :: zip' xs




