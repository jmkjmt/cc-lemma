{-

    All properties from the article.

-}
module Properties where

import HipSpec
import Prelude(Bool(..))
import Definitions

prop_1 :: [a] -> Prop [a]
prop_1 lst = ta1 lst =:= ta2 lst

prop_2 :: [a] -> Prop [a]
prop_2 lst = ta1 lst =:= ta3 lst

prop_3 :: [a] -> Prop [a]
prop_3 lst = ta1 lst =:= ta4 lst

prop_4 :: [a] -> Prop [a]
prop_4 lst = ta1 lst =:= sol4 lst

prop_5 :: [a] -> Prop [a]
prop_5 lst = ta1 lst =:= sol5 lst

prop_6 :: [a] -> Prop [a]
prop_6 lst = ta1 lst =:= sol9 lst

prop_7 :: [a] -> Prop [a]
prop_7 lst = ta1 lst =:= sol20 lst

prop_8 :: [a] -> Prop [a]
prop_8 lst = ta1 lst =:= sol43 lst

prop_9 :: [a] -> Prop [a]
prop_9 lst = ta1 lst =:= sol57 lst

prop_10 :: [a] -> Prop [a]
prop_10 lst = ta1 lst =:= sol75 lst

prop_11 :: [a] -> Prop [a]
prop_11 lst = ta1 lst =:= sol83 lst

prop_12 :: [a] -> Prop [a]
prop_12 lst = ta1 lst =:= sol89 lst

prop_13 :: [a] -> Prop [a]
prop_13 lst = ta1 lst =:= sol101 lst