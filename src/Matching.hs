module Matching(match) where

import term
import Substitution
import Data.Maybe

-- Create a substitution from the first term to the second term
match :: Term -> Term -> Maybe Subst
match (Var varName) term = Just (single varName term)
match _             (Var _) = Nothing
-- WIP
match (Comb combName1 list1) (Comb combName2 list2) =
