{-# LANGUAGE DeriveDataTypeable, FlexibleInstances #-}
{-

    Definitions for the properties in Productive Use Of Failure

-}
module Definitions where

import Prelude (Eq,Ord,Show,iterate,(!!),fmap,return,Bool(..))
import HipSpec

-- Booleans

otherwise = True

True && x = x
_    && _ = False

False || x = x
_     || _ = True

not True = False
not False = True

-- Nats

data Nat = S Nat | Z deriving (Eq,Show,Typeable,Ord)

instance Arbitrary Nat where
    arbitrary =
        let nats = iterate S Z
        in  (nats !!) `fmap` choose (0,5)

instance Partial Nat where
    unlifted Z     = return Z
    unlifted (S x) = fmap S (lifted x)

instance Names Nat where
    names _ = ["m","n","o"]

instance Names (A -> A) where
    names _ = ["f","g","h"]

instance Names (A -> Bool) where
    names _ = ["p","q","r"]

-- formula

data Formula
  = Truef
  | Falsef
  | Not Formula
  | AndAlso Formula Formula
  | OrElse Formula Formula
  | Imply Formula Formula
  | Equal Exp Exp

data Exp
  = Num Nat
  | Plus Exp Exp
  | Minus Exp Exp

(+) :: Nat -> Nat -> Nat
Z + y = y
(S x) + y = S (x + y)

(*) :: Nat -> Nat -> Nat
Z * _ = Z
(S x) * y = y + (x * y)

(-) :: Nat -> Nat -> Nat
Z - _ = Z
(S x) - y = S (x - y)

(==),(/=) :: Nat -> Nat -> Bool
Z   == Z   = True
Z   == _   = False
S _ == Z   = False
S x == S y = x == y

x /= y = not (x == y)

(<=) :: Nat -> Nat -> Bool
Z   <= _   = True
_   <= Z   = False
S x <= S y = x <= y

one, zero :: Nat
zero = Z
one  = S Z

double :: Nat -> Nat
double Z     = Z
double (S x) = S (S (double x))

even :: Nat -> Bool
even Z         = True
even (S Z)     = False
even (S (S x)) = even x

half :: Nat -> Nat
half Z         = Z
half (S Z)     = Z
half (S (S x)) = S (half x)

mult :: Nat -> Nat -> Nat -> Nat
mult Z     _ acc = acc
mult (S x) y acc = mult x y (y + acc)

fac :: Nat -> Nat
fac Z     = S Z
fac (S x) = S x * fac x

qfac :: Nat -> Nat -> Nat
qfac Z     acc = acc
qfac (S x) acc = qfac x (S x * acc)

exp :: Nat -> Nat -> Nat
exp _ Z     = S Z
exp x (S n) = x * exp x n

qexp :: Nat -> Nat -> Nat -> Nat
qexp x Z     acc = acc
qexp x (S n) acc = qexp x n (x * acc)

-- Lists

length :: [a] -> Nat
length []     = Z
length (_:xs) = S (length xs)

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

drop :: Nat -> [a] -> [a]
drop Z     xs     = xs
drop _     []     = []
drop (S x) (_:xs) = drop x xs

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

qrev :: [a] -> [a] -> [a]
qrev []     acc = acc
qrev (x:xs) acc = qrev xs (x:acc)

revflat :: [[a]] -> [a]
revflat []           = []
revflat (xs:xss)     = revflat xss ++ xs

qrevflat :: [[a]] -> [a] -> [a]
qrevflat []           acc = acc
qrevflat (xs:xss)     acc = qrevflat xss (rev xs ++ acc)

rotate :: Nat -> [a] -> [a]
rotate Z     xs     = xs
rotate _     []     = []
rotate (S n) (x:xs) = rotate n (xs ++ [x])

elem :: Nat -> [Nat] -> Bool
elem _ []     = False
elem n (x:xs) = n == x || elem n xs

subset :: [Nat] -> [Nat] -> Bool
subset []     ys = True
subset (x:xs) ys = x `elem` ys && subset xs ys


intersect :: [Nat] -> [Nat] -> [Nat]
(x:xs) `intersect` ys | x `elem` ys = x:(xs `intersect` ys)
                      | otherwise = xs `intersect` ys
[] `intersect` ys = []

union :: [Nat] -> [Nat] -> [Nat]
union (x:xs) ys | x `elem` ys = union xs ys
                | otherwise = x:(union xs ys)
union [] ys = ys

isort :: [Nat] -> [Nat]
isort [] = []
isort (x:xs) = insert x (isort xs)

insert :: Nat -> [Nat] -> [Nat]
insert n [] = [n]
insert n (x:xs) =
  case n <= x of
    True -> n : x : xs
    False -> x : (insert n xs)

count :: Nat -> [Nat] -> Nat
count n (x:xs) | n == x = S (count n xs)
               | otherwise = count n xs
count n [] = Z

sorted :: [Nat] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _        = True

ta1_remove_elem :: a -> [a] -> [a]
ta1_remove_elem e [] = []
ta1_remove_elem e (hd:tl) = if e == hd then ta1_remove_elem e tl else hd : ta1_remove_elem e tl

ta1 :: [a] -> [a]
ta1 [] = []
ta1 (hd:tl) = hd : (ta1_remove_elem hd (ta1 tl))

ta2 :: [a] -> [a]
ta2 [] = []
ta2 (hd:tl) = hd : (ta2 (ta1_remove_elem hd tl))

ta3_is_in :: [a] -> a -> Bool
ta3_is_in [] e = False
ta3_is_in (hd:tl) e = if e == hd then True else ta3_is_in tl e

ta3_uniq :: [a] -> [a] -> [a]
ta3_uniq [] lst2 = lst2
ta3_uniqe (hd:tl) lst2 = if ta3_is_in lst2 hd then ta3_uniq tl lst2 else ta3_uniq tl (lst2 ++ [hd])

ta3 :: [a] -> [a]
ta3 lst = ta3_uniq lst []

ta4_isnotin :: [a] -> a -> Bool
ta4_isnotin [] e = True
ta4_isnotin (hd:tl) e = if e == hd then False else ta4_isnotin tl e

ta4_uniq :: [a] -> [a] -> [a]
ta4_uniq [] lst2 = lst2
ta4_uniq (hd:tl) lst2 = if ta4_isnotin lst2 hd then ta4_uniq tl (lst2 ++ [hd]) else ta4_uniq tl lst2

ta4 :: [a] -> [a]
ta4 lst = ta4_uniq lst []

sol4_comb :: [a] -> a -> [a]
sol4_comb [] a = [a]
sol4_comb (hd:tl) a = if hd == a then hd:tl else hd : (sol4_comb tl a)

sol4_app :: [a] -> [a] -> [a]
sol4_app [] lst2 = lst2
sol4_app (hd:tl) lst2 = sol4_app tl (comb lst2 hd)

sol4 :: [a] -> [a]
sol4 lst = sol4_app lst []

sol5_del :: [a] -> a -> [a]
sol5_del [] a = []
sol5_del (hd:tl) a = if hd == a then sol4_del tl a else hd:(sol5_del tl a)

sol5 :: [a] -> [a]
sol5 [] = []
sol5 (hd:tl) = if (ta4_isnotin tl hd) then hd:(sol5 tl) else hd: (sol5 (sol5_del tl hd))

sol9_fastrev :: [a] -> [a]
sol9_fastrev lst = 
    let
        sol9_rev [] acc = acc
        sol9_rev (hd:tl) acc = sol9_rev tl ([hd] ++ acc)
    in
        sol9_rev lst []

sol9_delete :: [a] -> [a]
sol9_delete [] = []
sol9_delete (hd:tl) a = if ta3_is_in tl hd then sol9_delete tl else [hd] ++ (sol9_delete tl)

sol9 :: [a] -> [a]
sol9 lst = sol9_fastrev (sol9_delete (sol9_fastrev lst))

sol20_find :: a -> [a] -> Bool
sol20_find e [] = False
sol20_find e (hd:tl) = (e==hd)||(sol20_find e tl)

list_rev :: [a] -> [a]
list_rev lst = 
    let
        list_append [] acc = acc
        list_append (hd:tl) acc = list_append tl ([hd] ++ acc)
    in
        list_rev lst []

sol20 :: [a] -> [a]
sol20 lst =
    let
        rev_list = list_rev lst
    in
        case rev_list of
            [] -> []
            (hd:tl) -> if sol20_find hd tl then sol20 (list_rev tl) else (sol20 (list_rev tl)) ++ [hd]

