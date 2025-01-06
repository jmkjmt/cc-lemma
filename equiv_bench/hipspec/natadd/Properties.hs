{-

    All properties from the article.

-}
module Properties where

import HipSpec
import Prelude(Bool(..))
import Definitions

prop_1 :: Nat -> Nat -> Prop Nat
prop_1 n1 n2 = ta1 n1 n2 =:= ta2 n1 n2

prop_2 :: Nat -> Nat -> Prop Nat
prop_2 n1 n2 = ta1 n1 n2 =:= ta3 n1 n2

prop_3 :: Nat -> Nat -> Prop Nat
prop_3 n1 n2 = ta1 n1 n2 =:= ta3 n1 n2

prop_4 :: Nat -> Nat -> Prop Nat
prop_4 n1 n2 = ta1 n1 n2 =:= ta4 n1 n2

prop_5 :: Nat -> Nat -> Prop Nat
prop_5 n1 n2 = ta1 n1 n2 =:= sol4 n1 n2

prop_6 :: Nat -> Nat -> Prop Nat
prop_6 n1 n2 = ta1 n1 n2 =:= sol6 n1 n2

prop_7 :: Nat -> Nat -> Prop Nat
prop_7 n1 n2 = ta1 n1 n2 =:= sol11 n1 n2

--prop_8 :: Nat -> Nat -> Prop Nat
--prop_8 n1 n2 = ta1 n1 n2 =:= sol22 n1 n2

prop_9 :: Nat -> Nat -> Prop Nat
prop_9 n1 n2 = ta1 n1 n2 =:= sol143 n1 n2

