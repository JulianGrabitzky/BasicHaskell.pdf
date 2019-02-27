{-# LANGUAGE TemplateHaskell #-}

import PrettyPrinting
import Test.QuickCheck

-- prop_S

return []
testAll = $quickCheck
