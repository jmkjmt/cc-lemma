{-

    All properties from the article.

-}
module Properties where

import HipSpec
import Prelude(Bool(..))
import Definitions

-- Theorems
prop_1 :: Formula -> Prop Formula
prop_1 f = ta1 f =:= sol441 f