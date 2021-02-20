data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving Show

someTree :: Tree String
someTree = Node"t1"
              (Node "t2a"
                (Node "t3a" Leaf Leaf)
                (Node "t3b" Leaf Leaf))
              (Node "t2b"
                (Node "t3c" Leaf Leaf)
                (Node "t3d" Leaf Leaf))



mapTree :: (a->b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node value left right) = Node (f value) (mapTree f left) (mapTree f right)

flipTree :: Tree a -> Tree a
flipTree Leaf = Leaf
flipTree (Node value left right) = Node (value) (flipTree right) (flipTree left)

inorderTraversal :: Tree a -> [a]
inorderTraversal Leaf = []
inorderTraversal (Node value Leaf Leaf) = [value]
inorderTraversal (Node value left right) = [] ++ inorderTraversal left ++ [value] ++ inorderTraversal right

getLevel :: Int -> Tree a -> [a]
getLevel num Leaf = []
getLevel 0 (Node value left right) = [value]
getLevel num (Node value left right) = [] ++ getLevel (num - 1)(left) ++ getLevel (num - 1)(right)
{-- getLevel 9 someTree returns type of someTree :: String --}


mylast :: [a] -> Maybe a
mylast [] = Nothing
mylast (x:[]) = Just x
mylast (x:xs) = mylast xs

data Graph = GraphNode Int [Graph]

graph1 = GraphNode 1 [
            GraphNode 2 [GraphNode 5 []],
            GraphNode 3 [],
            GraphNode 4 [GraphNode 6 [GraphNode 7 [], GraphNode 8 []]]
            ]
graph2 = GraphNode 1 [
            GraphNode 2 [GraphNode 5 []],
            GraphNode 3 [],
            GraphNode 4 [GraphNode 6 [GraphNode 7 [graph2], GraphNode 8 []]]
            ]
graph3 = GraphNode 1 []
