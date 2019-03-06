module Reduction where

import Data.Maybe
import Matching
import Position
import Prog
import Substitution
import Term

-- Find a rule for a term in a given program.
findRule :: Prog  -- ^ List of rules
         -> Term  -- ^ Term to be replaced
         -> Maybe (
              Rhs,   -- ^ Right side of matched rule
              Subst  -- ^ Substitution from the rules' left side to the term
            )
findRule (Prog [])                     _    = Nothing
findRule (Prog ((Rule left right):rs)) term = case match left term of
  Nothing  -> findRule (Prog rs) term
  Just sub -> Just (right, sub)

-- Reduce a term at the given position.
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt (Prog []) _    _   = Nothing
reduceAt prog      term pos = case findRule prog (selectAt term pos) of
  Nothing           -> Nothing
  Just (rhs, subst) -> Just (replaceAt term pos (apply subst rhs))

-- Find all reducable positions of the term.
reduciblePos :: Prog -> Term -> [Pos]
reduciblePos (Prog []) _    = []
-- Take the first element of each tuple that is not Nothing.
reduciblePos prog      term
  = fst $ unzip $ filter (\x -> isJust (snd x)) positionAndRule
 where
  -- List of all positions of the term and possible rule.
  positionAndRule = zip (allPos term) (map (findRule prog) subterms)
  -- List of all subterms.
  subterms = map (selectAt term) (allPos term)

-- Determine if a term is in its normal form.
isNormalForm :: Prog -> Term -> Bool
isNormalForm prog term = null $ reduciblePos prog term
