import Data.List
import Data.SortedSet

pikkus : Type
pikkus = Double

-- data List a = Nil | (::) a (List a)

data MyList a = Nil | (::) a (List a)

test_list : MyList Int
test_list = [0,1,2] 

data Lam =
     Var String
    | App Lam Lam
    | Abs String Lam

showLam : Nat -> Lam -> String
showLam _ (Var x) = x
showLam d (App f e) = showParens (d)