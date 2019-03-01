module PrettyPrinting where

import Term

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  -- make it fancy
  pretty term | head (prettyHelper term) == '(' = middle (prettyHelper term)
              | otherwise                       = prettyHelper term
    where
      -- middle :: [String] -> [String]
      -- remove first and last element of a list
      middle [] = []
      middle [x] = []
      middle xs = tail (init xs)
      -- prettyHelper :: [Term] -> String
      -- make a list look pretty
      prettyHelper (Var name)       = name
      prettyHelper (Comb name [])   = name
      prettyHelper (Comb name args) = "(" ++ name ++ " " ++ (prettyList args) ++ ")"
      -- prettyList :: [Term] -> String
      -- use pretty on every element on the list
      prettyList list = unwords (map prettyHelper list)
