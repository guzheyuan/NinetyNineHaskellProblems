mySlice :: [a] -> Int -> Int -> [a]
mySlice xs s e = take (e - s + 1) $ drop (s - 1) xs 

