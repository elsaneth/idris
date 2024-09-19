module K2
import Data.List
import Data.SnocList
import Data.String
import Data.Nat
import Data.Monoid.Exponentiation

{-
Funktsioon fst' - paari esimene element
-}

fst' : (a, b) -> a
fst' (x, y) = x

{-
Funktsioon length' - listi pikkuse arvutamine
-}

length' : List a -> Int
length' [] = 0
length' (x::xs) = 1 + length' xs

{-
Standardteegis on listide konkateneerimiseks operaator ++.
(Ülakomad pole operaatorite nimedes lubatud)
-}

-- varjame sisseehitatud konkateneerimise arvutuse
infixr 7 +++
(+++) : List a -> List a -> List a
(+++) [] ys = ys
-- (+++) (x::xs) ys = (x::xs) ++ ys
(+++) (x::xs) ys = x::(xs+++ys)

{-
Funktsioon replicate' - n-st elemendist x 
koosneva listi konstrueerimine
-}

replicate' : Int -> a -> List a
replicate' 0 y = []
replicate' x y = y :: replicate' (x - 1) y

{-
Funktsioon take' - listist algusosa võtmine
-}

take' : Int -> List a -> List a
take' 0 y = []
take' x [] = []
take' x (y::ys) = y :: take' (x - 1) ys

{-
Funktsioon sum' - listi elementide summa
-}
-- TODO tee see funktsioon korda
-- moodles error: ✘ sum' (0 / 2) ebaõnnestus: Main.sum' [3,2] == 5

sum' : List Int -> Int
sum' [] = 0
sum' (y::ys) = y + sum' ys

{-
Funktsioon drop' - listi algusosa eemalejätmine
-}

drop' : Int -> List a -> List a
drop' 0 y = y
drop' x [] = []
drop' x (y::ys) = drop' (x-1) ys

{-
 Funktsioon reverse' - listi ümberpööramine
-}

reverse' : List a -> List a
reverse' [] = []
reverse' (y::ys) = reverse' ys +++ [y]

{-
Funktsioon tagastab paaride listi esimesed elemendid (samas järjekorras).
-}

esimesed : List (a, b) -> List a
esimesed [] = []
esimesed ((x, y) :: xs) = x :: esimesed xs

{-
Tagastab True, kui arv leidub listis, muidu False.
-}

leidub : Integer -> List Integer -> Bool
leidub n [] = False
leidub n (x::xs) = if n == x then True else leidub n xs

{-
Eemaldab listi viimase elemendi
-}

dropLast : List a -> List a
dropLast [] = []
dropLast [x] = []
dropLast (x :: xs) = x :: dropLast xs

{-
Lisab tähe sõnesse. Kusjuures nulli või negatiivse indeksiga 
pannakse täht algusse, suurema indeksiga, kui sõne pikkus, lõppu.
-}

lisa : Integer -> Char -> String -> String
lisa i x ys = 
    let tahed = unpack ys in
    let pikkus = length tahed in
    if i <= 0 
        then pack([x] ++ unpack ys) 
    else if i >= fromInteger (natToInteger pikkus)
        then pack(unpack ys ++ [x])
    else 
        let (a, b) = splitAt (fromInteger i) tahed in
        pack (a ++ [x] ++ b)

{-
Väärtustab polünoomi kohal x. Iga listi element (a, n) tähistab polynoomi liidetavat a*x^n.
-}

arvuta : List (Double, Nat) -> Double -> Double
arvuta ps x = sum (map (\(a, n) => a * (x ^ (n))) ps)
