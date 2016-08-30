compress :: Eq a => [a] -> [a]
compress = foldl skip []
    where skip [] x = [x]
          skip acc x
                | x == last acc = acc
                | otherwise = acc ++ [x]