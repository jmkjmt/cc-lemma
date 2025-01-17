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

expEval :: Exp -> Nat
expEval exp =
    case exp of
        Num n -> n
        Plus e1 e2 -> expEval e1 + expEval e2
        Minus e1 e2 -> expEval e1 - expEval e2

ta1 :: Formula -> Bool
ta1 f =
    case f of
        Truef -> True
        Falsef -> False
        Not f -> not (ta1 f)
        AndAlos f1 f2 -> ta1 f1 && ta1 f2
        OrElse f1 f2 -> ta1 f1 || ta1 f2
        Imply f1 f2 -> not (ta1 f1) || ta1 f2
        Equal e1 e2 -> expEval e1 == expEval e2

eval_help :: Formula -> Formula
eval_help f =
    case f of
        Truef -> Truef
        Falsef -> Falsef
        Not Truef -> Falsef
        Not Falsef -> Truef
        Not f -> Not (eval_help f)
        AndAlso Truef Truef -> Truef
        AndAlso Truef Falsef -> Falsef
        AndAlso Falsef f -> Falsef
        AndAlso f1 f2 -> eval_help (AndAlso (eval_help f1) (eval_help f2))
        OrElse Truef f -> Truef
        OrElse Falsef Truef -> Truef
        OrElse Falsef Falsef -> Falsef
        OrElse f1 f2 -> eval_help (OrElse (eval_help f1) (eval_help f2))
        Imply Falsef f -> Truef
        Imply Truef Falsef -> Falsef
        Imply Truef Truef -> Truef
        Imply f1 f2 -> eval_help (Imply (eval_help f1) (eval_help f2))
        Equal e1 e2 -> | expEval e1 == expEval e2 = Truef
                       | otherwise = Falsef
convert :: Formula -> Bool
convert f = 
    case f of
        Truef -> True
        Falsef -> False

sol441 :: Formula -> Bool
sol441 f = | convert (eval_help f) = True
           | otherwise = False