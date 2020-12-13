-- Performs tree sort algorithm

data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving Show

insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y l r)
    | x >= y = Node y l (insert x r)
    | otherwise = Node y (insert x l) r

buildtree :: Ord a => [a] -> Tree a
buildtree [] = Empty
buildtree (x:xs) = insert x (buildtree xs)

iotraversal :: Ord a => Tree a -> [a]
iotraversal Empty = []
iotraversal (Node x l r) = iotraversal l ++ [x] ++ iotraversal r

-- inserts all the to-be-sorted data items into a bsearch tree, then performs
-- an inorder traversal to extract the items in sorted order.
treesort :: Ord a => [a] -> [a]
treesort xs = iotraversal (buildtree xs)