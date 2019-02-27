module Substitution where

data Subst = Subst (Term -> Term)

instance Functor Subst where
    -- fmap :: (Term -> Term) -> Subst Term -> Subst Term
    fmap 