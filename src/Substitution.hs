module Substitution where

import Term

-- Substitution of two terms.
type Subst = (VarName -> Term)

-- Return the substitution itself.
identity :: Subst
identity = \x -> (Var x)

-- Substitute a variable with a term.
single :: VarName -> Term -> Subst
single varName term = (\x -> if x == varName then term else Var x)

-- Apply a Substitution to a term.
apply :: Subst -> Term -> Term
apply sub (Var varName)        = sub varName
apply sub (Comb combName list) = Comb combName (map (apply sub) list)

-- Apply two substitutions one after another.
compose :: Subst -> Subst -> Subst
compose s1 s2 = \varName -> apply s2 (s1 varName)
