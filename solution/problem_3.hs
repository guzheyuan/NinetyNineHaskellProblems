elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) k 
    | k < 1 = error "Index out of bounds" 
    | otherwise = elementAt xs (k - 1)