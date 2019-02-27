module Position where

type Pos = ""

above :: Pos -> Pos -> Bool

below :: Pos -> Pos -> Bool

leftOf :: Pos -> Pos -> Bool

rightOf :: Pos -> Pos -> Bool

selectAt :: Term -> Pos -> Term

replaceAt :: Term -> Pos -> Term -> Term

allPos :: Term -> [Pos]
