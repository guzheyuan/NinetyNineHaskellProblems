myRotate :: [a] -> Int -> [a]
myRotate xs n = drop n xs ++ take n xs
