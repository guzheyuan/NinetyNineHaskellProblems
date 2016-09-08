dupli :: [a] -> Int -> [a]
dupli xs n = foldl (\acc e -> acc ++ replicate n e) [] xs
