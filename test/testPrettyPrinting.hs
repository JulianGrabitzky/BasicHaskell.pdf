{-# LANGUAGE TemplateHaskell #-}

import PrettyPrinting
import Test.QuickCheck

prop_PrettyPrinting :: Term -> Bool

return []
testAll = $quickCheck
