myDrop :: [a] -> Int -> [a]
myDrop [] _ = []
myDrop (x:xs) 1 = xs
myDrop (x:xs) n = x : myDrop xs (n - 1)  
