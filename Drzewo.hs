data Tree a = EmptyTree 
                | Node a (Tree a) (Tree a)
                deriving (Eq, Ord, Read, Show)

emptyTree :: a -> Tree a  
emptyTree a = Node a EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = emptyTree x
treeInsert x  (Node a left right) 
        | x == a = (Node x left right)
        | x < a =  (Node a (treeInsert x left) right)   
        | x > a =  (Node a left (treeInsert x right))


fillTree :: Int -> Tree Int -> Tree Int
fillTree  1000 tree = tree 
fillTree  x tree = let a = treeInsert x tree
                   in fillTree (x + 1) a 
pokaz :: Tree a -> [Char]
pokaz EmptyTree = " b "
pokaz (Node a r l) = " a " ++ pokaz r ++ pokaz l
