myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- myLength = foldr (const (+1)) 0
