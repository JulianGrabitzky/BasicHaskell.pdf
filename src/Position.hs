module Position where

import Term

type Pos = [Int]

-- determine if a posiiton is below another
-- regarding a common predecessor
above :: Pos -> Pos -> Bool
above _  [] = False
above [] _  = True
above (p1:p1s) (p2:p2s) | p1 /= p2  = False
                        | otherwise = above p1s p2s

-- determine if a posiiton is below another
-- regarding a common predecessor
below :: Pos -> Pos -> Bool
below p1 p2 = above p2 p1

-- determine if a position is left of another position
leftOf :: Pos -> Pos -> Bool
leftOf []       _        = False
leftOf _        []       = False
leftOf (p1:p1s) (p2:p2s) | p1 <  p2  = True
                         | p1 >  p2  = False
                         | otherwise = leftOf p1s p2s

-- determine if a position is right of another position
rightOf :: Pos -> Pos -> Bool
rightOf []       _        = False
rightOf _        []       = False
rightOf (p1:p1s) (p2:p2s) | p1 <  p2  = False
                          | p1 >  p2  = True
                          | otherwise = rightOf p1s p2s

-- select a partial term on a position
selectAt :: Term -> Pos -> Term
selectAt t             []     = t
selectAt (Var _)       _      = error "The position you are looking for does not exist."
selectAt (Comb _ args) (p:ps) = selectAt (args !! (p-1)) ps

-- replace a partial term on a position
replaceAt :: Term -> Pos -> Term -> Term
replaceAt _             []     t2 = t2
replaceAt (Var _)       _      _  = error "The position you want to replace does not exist."
replaceAt (Comb c args) (p:ps) t  | length args < p = error "The position you want to replace does not exist."
                                  | otherwise       = (Comb c newArgs)
    where newArgs         = firstElements ++ replacedElement ++ lastElements
          firstElements   = (take (p-1) args)
          replacedElement = [replaceAt (args !! (p-1)) ps t]
          lastElements    = drop p args

-- return all positions of a term
allPos :: Term -> [Pos]
allPos (Var _) = [[]]
allPos (Comb _ args) = [] : concatMap helper [1..(length args)]
    where helper pos = map (pos:) (allPos $ args !! (pos-1))
