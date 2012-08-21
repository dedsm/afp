elementAt :: [a] -> Int -> a
elementAt [] _ = error "List not big enough"
elementAt (x:_) 1 = x
elementAt (_:xs) n 
    | n >= 1 = elementAt xs (n-1)
    | otherwise = error "Invalid index"
