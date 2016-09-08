before :: [a] -> Int -> [a]
before [] _ = []
before xs 0 = []
before (x:xs) n = x : before xs (n-1) 

after :: [a] -> Int -> [a]
after [] _ = []
after xs 0 = xs
after (x:xs) n = after xs (n-1) 

mySplit :: [a] -> Int -> ([a],[a])
mySplit [] _ = ([],[])
mySplit xs n = (before xs n, after xs n)
