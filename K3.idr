import Data.List

{-
Leia termidest vabad muutujad!
-}

-- FV(λf. (λg. g x) (λx. f x)) = {x}
-- FV((λx. (λg. g x)) x) = {x}
-- FV(λg. (λf. f x) (λx. g x)) = {x}
-- FV((λy. y) (λf x. y x) (λx. g x)) = {y, g}
-- FV(z λh x. (λf. h f) x) = {z}

{-
Leia substitutsiooni tulemus!
-}

-- (λf. f y(λx.x))[y→λx y. f x] = (λz. z(λx y. fx)(λx. x))
-- (λx. f (x x))(λx. f (x x))[f→λx y. y] = (λx. (λ x y. y) (x x))(λx. (λx y. y) (x x))
-- (λx g y.x g y)[g→x g y] = (λ x g y. x g y)

{-
Kasutades listikomprehensiooni, 
leia kõik arvud mis on väiksemad kui 1000 ja jaguvad seitsmega.
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
zip' (x::xs) (y::ys) = (x, y) :: zip' xs ys

{-
Kasutades funktsiooni zip', kirjuta funktsioon mis 
leiab parameetrina antud listist kõik järjestikuste elementide paarid.
-}

pairs : List a -> List (a,a)
pairs xs = zip' xs (drop 1 xs)

{-
Kirjuta lihtrekursiivne funktsioon, 
mis kontrollib kas kõik listis olevad väärused on tõesed.
-}

and' : List Bool -> Bool
and' [] = True
and' (x::xs) = if x == False then False else and' xs

{-
Kasutades listikomprehensiooni ja eelnevalt defineeritud abifunktsiooni 
and' ning pairs, et kontrollida, kas parameetrina antud list on 
sorteeritud -- elemendid mittekahanevas järjekorras 
-}

sorted : List Int -> Bool
sorted xs = and' [x <= y | (x, y) <- pairs xs]

{-
Positiivsete täisarvude kolmik (a,b,c) on Pythagorase kolmik, 
kui kehtib võrdus a2+b2=c2.

Kasutades listikomprehensiooni, leia kõik Pythagorase kolmikud, 
mille komponendid on võrdsed või väiksemad kui etteantud parameeter.
-}

pyths : Int -> List (Int,Int,Int)
pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a * a + b * b == c * c]

{-
Täisarv n on täiuslik kui kõik tema täisarvuliste faktorite (v.a. arv ise) summa on arv n. Näiteks 6 saab jagada kolmega, kahega ja ühega ning 3+2+1=6.

Kasutades listikomprehensiooni, leia kõik täiuslikud arvud, mis on võrdsed või väiksemad kui etteantud parameeter.
-}

dropLast : List a -> List a
dropLast [] = []
dropLast [x] = []
dropLast (x :: xs) = x :: dropLast xs

perfects : Int -> List Int
perfects 0 = []
perfects n = [(a) | a <- [1..n],  sum (dropLast(factors a)) == a]

{-
Optimeeri Pythagorase kolmikute ülesande lahendust selliselt, 
et arvutatakse vaid kolmikud (a,b,c) kus a<b. 
Pane tähele, et lisakontrolli lisamine pole optimieerimine 
-- see teeb lahenduse ju aeglasemaks. 
Mõelge, kuidas saaks piirata proovitavate kolmikute arvu
-}

pythsOpt : Int -> List (Int,Int,Int)
pythsOpt n = [(a, b, c) | a <- [1..n], b <- [a..n], c <- [1..n], a * a + b * b == c * c]
