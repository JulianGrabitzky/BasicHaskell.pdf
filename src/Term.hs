module Term
( VarName, CombName, Term(..)
) where

import Data.List
  
-- Alias type for variable names.
-- A variable name is a string.
type VarName = String

-- Alias type for constructor and function names.
-- A constructor or function name is a string.
type CombName = String

-- Data type for terms.
-- A term is either a variable,
-- or a constructor or function applied to a list of terms.
data Term = Var VarName | Comb CombName [Term]
  deriving (Eq, Show)

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  -- make it fancy
  pretty term | head (prettyHelper term) == '(' = middle (prettyHelper term)
              | otherwise                     = prettyHelper term
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
      prettyList list = intercalate " " (map prettyHelper list)