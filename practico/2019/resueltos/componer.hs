componer :: String -> [String] -> [(String, String)]
componer s [] = []
componer s (x:xs) = (s,x) : componer s xs

{- componer "Juan" ["Perez", "Garcia"] = [("Juan","Perez"),("Juan","Garcia")]
componer "Pepe" [] = [] -}