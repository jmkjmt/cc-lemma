{-

    All properties from the article.

-}
module Properties where

import HipSpec
import Prelude(Bool(..))
import Definitions

-- Theorems
prop_1 :: [Nat] -> Prop [Nat]
prop_1 lst = ta1 lst =:= sol121 lst

prop_2 :: [Nat] -> Prop [Nat]
prop_2 lst = ta1 lst =:= sol171 lst
