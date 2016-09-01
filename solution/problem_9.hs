pack :: Eq a => [a] -> [[a]]
pack = foldl func [] 
    where func [] e = [[e]]
          func acc e
            | (head (last acc)) == e = (init acc) ++ [(last acc) ++ [e]] 
            | otherwise = acc ++ [[e]]