import Data.List 

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving Show

single :: a -> Tree a
single a = Node Leaf a Leaf

size :: Tree a -> Int
size Leaf = 0
size (Node left _ right) = 1 + size left + size right

height :: Tree a -> Int
height Leaf = 1
height (Node left _ right) = 1 + max (height left) (height right)

flatten :: Tree a -> [a]
flatten Leaf = []
flatten (Node left val right) = (flatten left) ++ [val] ++ (flatten right)

reverseTree :: Tree a -> Tree a
reverseTree Leaf = Leaf
reverseTree (Node left val right) = Node (reverseTree right) val (reverseTree left)

treesort :: Ord a => [a] -> Tree a
treesort [] = Leaf
treesort (x:xs) = Node (treesort left) x (treesort right) where
                    (left, right) = partition (<x) xs

bst :: Ord a => Tree a -> Bool
bst Leaf = True
bst (Node Leaf val Leaf) = True
bst (Node leftN@(Node _ left _) val rightN@(Node _ right _)) = val >= left && val <= right && bst leftN && bst rightN
bst (Node leftN@(Node _ left _) val Leaf )= val >= left &&  bst leftN 
bst (Node Leaf val rightN@(Node _ right _)) = val <= right && bst rightN
