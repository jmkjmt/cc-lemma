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

data Lambda
    = V String
    | P String Lambda
    | C Lambda Lambda

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

ta1_ismem :: [String] -> String -> Bool
ta1_ismem [] var = False
ta1_ismem (hd:tl) var = if hd == var then True else ta1_ismem tl var

ta1_subcheck :: Lambda -> [String] -> Bool
ta1_subcheck (V x) vars = ta1_ismem vars x
ta1_subcheck (P x e) vars = ta1_subcheck e (x:vars)
ta1_subcheck (C e1 e2) vars = ta1_subcheck e1 vars && ta1_subcheck e2 vars

ta1 :: Lambda -> Bool
ta1 e = ta1_subcheck e []

ta2_subcheck :: Lambda -> [String] -> Bool
ta2_subcheck (V x) vars = ta1_ismem vars x
ta2_subcheck (P x e) vars = ta2_subcheck e (x:vars)
ta2_subcheck (C e1 e2) vars = ta2_subcheck e1 vars && ta2_subcheck e2 vars

ta2 :: Lambda -> Bool
ta2 e =
    case e of
        V x -> False
        P x e -> ta2_subcheck e [x]
        C e1 e2 -> ta2 e1 && ta2 e2

ta3_subcheck :: Lambda -> [String] -> Bool
ta3_subcheck (V x) vars = ta1_ismem vars x
ta3_subcheck (P x e) vars = if ta1_ismem vars x then ta3_subcheck e vars else ta3_subcheck e (x:vars)
ta3_subcheck (C e1 e2) vars = ta3_subcheck e1 vars && ta3_subcheck e2 vars

ta3 :: Lambda -> Bool
ta3 e = ta3_subcheck e []

ta4_exists :: (String -> Bool) -> [String] -> Bool
ta4_exists p [] = False
ta4_exists p (hd:tl) = p hd || ta4_exists p tl

ta4_subcheck :: Lambda -> [String] -> Bool
ta4_subcheck (V x) vars = ta4_exists (\y -> x == y) vars
ta4_subcheck (P x e) vars = ta4_subcheck e (x:vars)
ta4_subcheck (C e1 e2) vars = ta4_subcheck e1 vars && ta4_subcheck e2 vars

ta4 :: Lambda -> Bool
ta4 e =
    case e of
        V x -> False
        P x e -> ta4_subcheck e [x]
        C e1 e2 -> ta4_subcheck e1 [] && ta4_subcheck e2 []

sol5 :: Lambda -> Bool
sol5 m =
    let 
        getStn (V var) = [var]
        getStn (P var e) = list_filter (\x -> x /= var) (getStn e)
        getStn (C e1 e2) = getStn e1 ++ getStn e2
    in
        if (getStn m) == [] then True else False

sol50 :: Lambda -> Bool
sol50 m =
    let
        valify var (V n) = False
        valify var (P n e) = if n == var then True else valify var e
        valify var (C m1 m2) = False
    in
    let 
        findStation (V n) covers = valify n covers
        findStation (P n e) covers = findStation e (P n covers)
        findStation (C m1 m2) covers = findStation m1 covers || findStation m2 covers
    in
        findStation m (V " ")

sol57 :: Lambda -> Bool
sol57 m =
    let
        deleteAll [] target = []
        deleteAll (hd:tl) target = if hd == target then deleteAll tl target else hd : deleteAll tl target
    in
    let
        listStation (V n) = [n]
        listStation (P n e) = deleteAll (listStation e) n
        listStation (C m1 m2) = listStation m1 ++ listStation m2
    in
        listStation m == []

sol101 :: Lambda -> Bool
sol101 m =
    let
        check_ (V n) al nl = list_forall (\x -> list_mem x al) (n:nl)
        check_ (P n e) al nl = check_ e (n:al) nl
        check_ (C m1 m2) al nl = check_ m1 al nl && check_ m2 al nl
    in
    check_ m [] []

sol109_varExists :: String -> [String] -> Bool
sol109_varExists var [] = False
sol109_varExists var (hd:tl) = if hd == var then True else sol109_varExists var tl

sol109_addToNameList :: String -> [String] -> [String]
sol109_addToNameList var lst = if sol109_varExists var lst then lst else var:lst

sol109_checkRec :: Lambda -> [String] -> Bool
sol109_checkRec (V var) lst = sol109_varExists var lst
sol109_checkRec (P var e) lst = sol109_checkRec e (sol109_addToNameList var lst)
sol109_checkRec (C e1 e2) lst = sol109_checkRec e1 lst && sol109_checkRec e2 lst

sol109 :: Lambda -> Bool
sol109 m = sol109_checkRec m []

sol123 :: Lambda -> Bool
sol123 m =
    let
        is_connect (V n) = False
        is_connect (P n e) = is_connect e
        is_connect (C e1 e2) = True
    in
    let
        get_left (V n) = V n
        get_left (P n e) = P n (get_left e)
        get_left (C e1 e2) = e1
    in
    let
        get_right (V n) = V n
        get_right (P n e) = P n (get_right e)
        get_right (C e1 e2) = e2
    in
    if is_connect m then (sol123 (get_left m) && sol123 (get_right m))
                    else case m of
                    V n -> False
                    C e1 e2 -> sol123 e1 && sol123 e2
                    P n e -> case e of
                                V s -> s == n
                                C e1 e2 -> sol123 (P n e1) && sol123 (P n e2)
                                P s e -> sol123 (P n e) || sol123 (P s e)

-- sol530 uses integer