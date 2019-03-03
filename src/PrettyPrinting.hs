module PrettyPrinting
    ( Pretty, pretty
    ) where

import Term

class Pretty a where
  pretty :: a -> String

-- sleek look for term
instance Pretty Term where
    pretty (Var name) = name
    pretty (Comb name []) = name
    pretty (Comb name list) = name ++ " " ++ (prettyList list)
        where
            -- prettyHelper :: [Term] -> String
            prettyHelper (Var varName)       = varName
            prettyHelper (Comb varName [])   = varName
            prettyHelper (Comb combName args) = "(" ++ combName ++ " " ++ (prettyList args) ++ ")"
            -- prettyList :: [Term] -> String
            -- use pretty on every element on the list
            -- and add whitespaces between the elements
            prettyList args = unwords (map prettyHelper args)
