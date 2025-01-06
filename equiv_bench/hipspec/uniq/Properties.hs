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
prop_5 n1 n2 = ta1 n1 n2 =:= ta5 n1 n2

prop_6 :: Nat -> Nat -> Prop Nat
prop_6 n1 n2 = ta1 n1 n2 =:= ta6 n1 n2

prop_7 :: Nat -> Nat -> Prop Nat
prop_7 n1 n2 = ta1 n1 n2 =:= ta7 n1 n2

prop_8 :: Nat -> Nat -> Prop Nat
prop_8 n1 n2 = ta1 n1 n2 =:= sol6 n1 n2


--prop_9 :: Nat -> Nat -> Prop Nat
--prop_9 n1 n2 = ta1 n1 n2 =:= sol22 n1 n2


prop_10 :: Nat -> Nat -> Prop Nat
prop_10 n1 n2 = ta1 n1 n2 =:= sol32 n1 n2


prop_11 :: Nat -> Nat -> Prop Nat
prop_11 n1 n2 = ta1 n1 n2 =:= sol84 n1 n2


prop_12 :: Nat -> Nat -> Prop Nat
prop_12 n1 n2 = ta1 n1 n2 =:= sol90 n1 n2


--prop_13 :: Nat -> Nat -> Prop Nat
--prop_13 n1 n2 = ta1 n1 n2 =:= sol116 n1 n2


prop_14 :: Nat -> Nat -> Prop Nat
prop_14 n1 n2 = ta1 n1 n2 =:= sol329 n1 n2


prop_15 :: Nat -> Nat -> Prop Nat
prop_15 n1 n2 = ta1 n1 n2 =:= sol353 n1 n2