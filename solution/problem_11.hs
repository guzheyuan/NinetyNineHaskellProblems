data ListItem a = Single a | Multiple Int a 
    deriving (Show)

encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = map (\e -> (length e,head e)) (foldl func [] $ xs)
    where func [] e = [[e]]
          func acc e
            | (head (last acc)) == e = (init acc) ++ [(last acc) ++ [e]] 
            | otherwise = acc ++ [[e]]


funcHelper :: (Int, a) -> ListItem a
funcHelper (c,x)
    | (c == 1) = Single x
    | otherwise = Multiple c x


encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified xs = map funcHelper $ encode xs