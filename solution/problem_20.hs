removeAt :: [a] -> Int -> (a,[a])
removeAt xs n = (xs!!(n-1), helpFunc xs n)
    where helpFunc [] _ = []
          helpFunc (x:xs) 1 = xs
          helpFunc (x:xs) n = x : helpFunc xs (n - 1)  
