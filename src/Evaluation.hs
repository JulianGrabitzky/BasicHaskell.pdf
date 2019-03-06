module Evaluation
  ( Strategy
  , loStrategy
  , liStrategy
  , roStrategy
  , riStrategy
  , poStrategy
  , piStrategy
  , reduceWith
  , evaluateWith
  ) where

import Position
import Prog
import Reduction
import Term

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

-- Leftmost outermost strategy
loStrategy :: Strategy
loStrategy = createStrategy above leftOf

-- Leftmost innermost strategy
liStrategy :: Strategy
liStrategy = createStrategy below leftOf

-- Rightmost outermost strategy
roStrategy :: Strategy
roStrategy = createStrategy above rightOf

-- Rightmost innermost strategy
riStrategy :: Strategy
riStrategy = createStrategy below rightOf

-- Parallel outermost strategy
poStrategy :: Strategy
poStrategy = createParStrategy above

-- Parallel innermost strategy
piStrategy :: Strategy
piStrategy = createParStrategy below

-- Look for the max elements in comparison
-- to every other element in the same subtree
createParStrategy :: (Pos -> Pos -> Bool)  -- ^ above/below testing in tree
                  -> Strategy
createParStrategy ab prog term
  = filter (ownFilter $ reduciblePos prog term) (reduciblePos prog term)
 where
  ownFilter :: [Pos] -> Pos -> Bool
  ownFilter [] _ = True
  ownFilter (p:ps) pos | ab p pos = False
                       | otherwise = ownFilter ps pos

-- Create a non-parallel evaluation strategy.
createStrategy :: (Pos -> Pos -> Bool)  -- ^ above/below testing in tree
               -> (Pos -> Pos -> Bool)  -- ^ left/right testing in tree
               -> Strategy
createStrategy ab lr prog term
  | isNormalForm prog term
  = []
  | otherwise
  = [helper (head (reduciblePos prog term)) (reduciblePos prog term)]
 where
  helper :: Pos -> [Pos] -> Pos
  helper p1 [] = p1
  helper p1 (p2:ps) | (ab p1 p2) || (lr p1 p2) = helper p1 ps
                    | otherwise                = helper p2 ps

-- Apply one reduction with the given strategy.
reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith strat prog term = reduceWithHelper (Just term) reduPosList
 where
  reduPosList = (strat prog term)
  reduceWithHelper :: Maybe Term -> [Pos] -> Maybe Term
  reduceWithHelper Nothing  _  = Nothing
  reduceWithHelper (Just t) [] = (Just t)
  reduceWithHelper (Just t) (p:ps) = reduceWithHelper (reduceAt prog t p) ps

-- Reduce the term with the given strategy until it reached normal form.
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith strat prog term
  | isNormalForm prog term = term
  | otherwise = evaluateWithHelper strat prog reducedTerm
 where
  reducedTerm = reduceWith strat prog term
  evaluateWithHelper :: Strategy -> Prog -> Maybe Term -> Term
  evaluateWithHelper s p (Just t)
    = evaluateWith s p t
  evaluateWithHelper _ _ _        
    = error "Error in Evaluation.hs: evaluateWith failed!"
