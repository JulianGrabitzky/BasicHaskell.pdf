{-# LANGUAGE TemplateHaskell #-}
module ArbitraryTerm where

import Term

instance Arbitrary Term where
    term = do


return []
testAll = $quickCheck
