module Substitution where

import Term

-- substitution of two terms
type Subst = (VarName -> Term)

-- just the id function my friend
identity :: Subst
identity = \x -> (Var x)

-- replace varName with a term
single :: VarName -> Term -> Subst
single varName term = (\x -> if x == varName then term else Var x)

-- apply the Substitution to given term
apply :: Subst -> Term -> Term
apply sub (Var varName)        = sub varName
apply sub (Comb combName list) = Comb combName (map (apply sub) list)

-- apply 2 substitutions one after another
compose :: Subst -> Subst -> Subst
compose s1 s2 = \varName -> apply s2 (s1 varName)
