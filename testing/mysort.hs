sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = insert x (sort xs)

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y:insert x ys
