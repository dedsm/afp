reverse :: [a] -> [a]
reverse [] = []
revese (x:xs) = reverse xs ++ [x]
