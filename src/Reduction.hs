module Reduction where

import Term
import Position
import Matching
import Substitution
import Prog
import Data.Maybe

-- make me pretty pls
findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])              term = Nothing
findRule (Prog ((Rule left right):rs)) term | left == term = Just (right, substitution)
                                            | otherwise    = findRule (Prog rs) term
    where substitution = allOrNothing (match term right)
          allOrNothing :: Maybe Subst -> Subst
          allOrNothing Nothing      = error "No reduction rules found."
          allOrNothing (Just subst) = subst

-- reduce the term at the given position
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt (Prog []) _    _   = Nothing
reduceAt prog      term pos = case findRule prog (selectAt term pos) of
    Nothing           -> Nothing
    Just (rhs, subst) -> Just (replaceAt term pos (apply subst rhs))

-- find all reducable positions of the term
reduciblePos :: Prog -> Term -> [Pos]
reduciblePos (Prog []) _ = []
-- take the first element of each tuple that is not Nothing
reduciblePos prog term = fst $ unzip $ filter (\x -> isJust (snd x)) (positionAndRule)
    where
        -- list of all positions of the term and possible rule
        positionAndRule = zip (allPos term) (map (findRule prog) subterms)
        -- list of all subterms
        subterms = map (selectAt term) (allPos term)

-- determine if a term is in its normal form
isNormalForm :: Prog -> Term -> Bool
isNormalForm prog term | reduciblePos prog term == 0 = True
                       | otherwise = False
