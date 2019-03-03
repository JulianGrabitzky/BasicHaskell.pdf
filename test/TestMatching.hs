-- Test function
applyMaybe :: Maybe Subst -> Term -> Term
applyMaybe (Just s) t = apply s t
applyMaybe Nothing  _ = error "No match found."
