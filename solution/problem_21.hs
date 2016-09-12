insertAt :: a -> [a] -> Int -> [a]
insertAt x xs i = take (i-1) xs ++ x : drop (i-1) xs

insertAt_ :: a -> [a] -> Int -> [a]
insertAt_ x xs n = let (ys,zs) = splitAt (n-1) xs in ys++x:zs