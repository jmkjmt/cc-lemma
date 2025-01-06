{-

    All properties from the article.

-}
module Properties where

import HipSpec
import Prelude(Bool(..))
import Definitions

-- Theorems
prop_1 :: Lambda -> Prop Lambda
prop_1 f = ta1 f =:= ta2 f

prop_2 :: Lambda -> Prop Lambda
prop_2 f = ta1 f =:= ta3 f

prop_3 :: Lambda -> Prop Lambda
prop_3 f = ta1 f =:= ta4 f

prop_4 :: Lambda -> Prop Lambda
prop_4 f = ta1 f =:= sol5 f

prop_5 :: Lambda -> Prop Lambda
prop_5 f = ta1 f =:= sol50 f

prop_6 :: Lambda -> Prop Lambda
prop_6 f = ta1 f =:= sol57 f

prop_7 :: Lambda -> Prop Lambda
prop_7 f = ta1 f =:= sol101 f

prop_8 :: Lambda -> Prop Lambda
prop_8 f = ta1 f =:= sol109 f

prop_9 :: Lambda -> Prop Lambda
prop_9 f = ta1 f =:= sol123 f

prop_10 :: Lambda -> Prop Lambda
prop_10 f = ta1 f =:= sol530 f
