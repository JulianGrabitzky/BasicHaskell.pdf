module Evaluation
    (Strategy, loStrategy, liStrategy, roStrategy, riStrategy,
     poStrategy, piStrategy, reduceWith, evaluateWith)
    where

import Reduction
import Position
import Term
import Prog

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

loStrategy :: Strategy
loStrategy = createStrategy above leftOf

liStrategy :: Strategy
liStrategy = createStrategy below leftOf

roStrategy :: Strategy
roStrategy = createStrategy above rightOf

riStrategy :: Strategy
riStrategy = createStrategy below rightOf

poStrategy :: Strategy
poStrategy = createParStrategy above

piStrategy :: Strategy
piStrategy = createParStrategy below

-- look for the max element in comparison to every other element in the same subtree
createParStrategy :: (Pos -> Pos -> Bool) -> -- above/below testing in tree
                     Strategy
createParStrategy ab prog term = filter (ownFilter $ reduciblePos prog term) (reduciblePos prog term)
  where
    ownFilter :: [Pos] -> Pos -> Bool
    ownFilter [] _ = True
    ownFilter (p:ps) pos | ab p pos = False
                         | otherwise = ownFilter ps pos

-- create non-parallel evaluation strategy
createStrategy :: (Pos -> Pos -> Bool) -> -- above/below testing in tree
                  (Pos -> Pos -> Bool) -> --  left/right testing in tree
                  Strategy
createStrategy ab lr prog term | isNormalForm prog term = []
                               | otherwise =
                                 [helper (head (reduciblePos prog term)) (reduciblePos prog term)]
    where
      helper :: Pos -> [Pos] -> Pos
      helper p1 [] = p1
      helper p1 (p2:ps) | (ab p1 p2) || (lr p1 p2) = helper p1 ps
                        | otherwise                = helper p2 ps

-- apply one reduction with the given strategy
reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith strat prog term = reduceWithHelper (Just term) reduPosList
    where
      reduPosList = (strat prog term)
      reduceWithHelper :: Maybe Term -> [Pos] -> Maybe Term
      reduceWithHelper Nothing  _  = Nothing
      reduceWithHelper (Just t) [] = (Just t)
      reduceWithHelper (Just t) (p:ps) = reduceWithHelper (reduceAt prog t p) ps

-- reduce the term with the given strategy until it reached normalform
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith strat prog term | isNormalForm prog term = term
                             | otherwise = evaluateWithHelper strat prog reducedTerm
    where
      reducedTerm = reduceWith strat prog term
      evaluateWithHelper :: Strategy -> Prog -> Maybe Term -> Term
      evaluateWithHelper s p (Just t) = evaluateWith s p t
      evaluateWithHelper _ _ _        = error "Error in Evaluation.hs: evaluateWith failed!"
