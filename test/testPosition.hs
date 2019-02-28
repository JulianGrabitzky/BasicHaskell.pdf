{-# LANGUAGE TemplateHaskell #-}

import Position
import Test.QuickCheck

return []
testAll = $quickCheck

t = (Comb "mul" [(Var "x"), (Comb "add" [(Var "y"), (Var "z"), (Var "w")])])