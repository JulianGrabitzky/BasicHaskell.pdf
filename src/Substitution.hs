module Substitution where

import Term

-- substitution of two terms
type Subst = (Term -> Term)

-- just the id function my friend
identity :: Subst
identity = id

-- replace varName with a term
single :: VarName -> Term -> Subst
single varName term = \varName -> term

-- apply the Substitution to given term
apply :: Subst -> Term -> Term
apply sub (Var varName)        = sub (Var varName)
apply sub (Comb combName list) = Comb combName (map (apply sub) list)

-- apply 2 substitutions one after another
compose :: Subst -> Subst -> Subst
compose s1 s2 = s2.s1
