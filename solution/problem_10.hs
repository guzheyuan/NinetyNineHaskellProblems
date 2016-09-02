encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = map (\e -> (length e,head e)) (foldl func [] $ xs)
    where func [] e = [[e]]
          func acc e
            | (head (last acc)) == e = (init acc) ++ [(last acc) ++ [e]] 
            | otherwise = acc ++ [[e]]