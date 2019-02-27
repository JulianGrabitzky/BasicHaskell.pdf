module Position where

import Term

type Pos = [Int]

above :: Pos -> Pos -> Bool
above p1 p2 | length p1 < length p2 = True
            | otherwise             = False

below :: Pos -> Pos -> Bool
below p1 p2 | length p1 > length p2 = True
            | otherwise             = False

-- determine if a position is left of another position
-- assumption: positions on same level
leftOf :: Pos -> Pos -> Bool
leftOf []       _        = False
leftOf _        []       = False
leftOf (p1:p1s) (p2:p2s) | p1 <  p2 = True
                         | p1 >  p2 = False
                         | p1 == p2 = leftOf p1s p2s

-- determine if a position is right of another position
-- assumption: positions on same level
rightOf :: Pos -> Pos -> Bool
rightOf []       _        = False
rightOf _        []       = False
rightOf (p1:p1s) (p2:p2s) | p1 <  p2 = False
                          | p1 >  p2 = True
                          | p1 == p2 = rightOf p1s p2s

selectAt :: Term -> Pos -> Term
selectAt t             []     = t
selectAt (Var v)       [pos]  = error "The position you are looking for does not exist." -- invalid input
selectAt (Comb c args) (p:ps) = selectAt (args !! (p-1)) ps

-- replaceAt :: Term -> Pos -> Term -> Term

-- allPos :: Term -> [Pos]
