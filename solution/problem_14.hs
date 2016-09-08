dupli :: [a] -> [a]
dupli xs = foldl (\acc e -> acc ++ [e,e]) [] xs
