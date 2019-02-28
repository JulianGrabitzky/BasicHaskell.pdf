{-# LANGUAGE TemplateHaskell #-}
module ArbitraryTerm where

import src/Term

instance Arbitrary Var where
    var = do
    a <- elements ['a'..'z']
    return Var a


return []
testAll = $quickCheck
