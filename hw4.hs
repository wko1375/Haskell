import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')

data Tree a = Node a (Tree a) (Tree a)
              | Leaf
              deriving Show

someTree :: Tree (Int, Int)
someTree = Node (7, 1)
            (Node (3, 1)
              (Node (1, 1) Leaf Leaf)
              (Node (2, 2) Leaf Leaf))
            (Node (10, 3)
              (Node (8, 1) Leaf Leaf)
              (Node (20, 1) Leaf Leaf))

otherTree :: Tree String
otherTree = Node "t1"
            (Node "t2a"
                (Node "t3a" Leaf Leaf)
                (Node "t3b" Leaf Leaf))
            (Node "t2b"
                (Node "t3c" Leaf Leaf)
                (Node "t3cc" Leaf Leaf))

{--frequency :: Ord a => [a] -> Map a Int
frequency [] = Map.empty
frequency [x] = (Map.insert x 1 Map.empty)
frequency [h:t] =
          if Map.lookup h m == Nothing
            then Map.insert h 1 m
            else Map.insert h ((Map.lookup h m) + 1) m
          where m = frequency t --}

instance Eq a => Eq (Tree a) where
    Leaf == Leaf = True
    (Node val left right) == Leaf = False
    Leaf == (Node val left right) = False
    (Node val1 left1 right1) == (Node val2 left2 right2) = (val1 == val2) && (left1 == left2) && (right1 == right2)

mapTree :: (a->b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node value left right) = Node (f value) (mapTree f left) (mapTree f right)

instance Functor (Tree) where
  fmap f (Node value left right) = mapTree f (Node value left right)

bstInsert :: Ord a => Tree (a, Int) -> a -> Tree (a, Int)
bstInsert Leaf num = (Node (num, 1) Leaf Leaf)
bstInsert (Node (key, val) left right) num
          | num == key = Node (key, val + 1) left right
          | num < key = Node (key, val) (bstInsert left num) right
          | num > key = Node (key, val) left (bstInsert right num)

phase1 = bstInsert Leaf "H"
phase2 = bstInsert phase1 "I"
phase3 = bstInsert phase2 "A"
phase4 = bstInsert phase3 "I"
phase5 = bstInsert phase4 "O"
phase6 = bstInsert phase5 "Z"

bstLookup :: Ord a => Tree (a, Int) -> a -> Int
bstLookup Leaf num = 0
bstLookup (Node (key, val) left right) num
          | num == key = val
          | num < key = bstLookup left num
          | num > key = bstLookup right num


{--findInorder :: Ord a => Tree (a, Int) -> Tree (a, Int)
findInorder (Node val Leaf Leaf) =  (Node val Leaf Leaf)
findInorder (Node val left Leaf) =  left
findInorder (Node val Leaf right) =  right
findInorder (Node val left right) = findInorder right

bstRemove :: Ord a => Tree (a, Int) -> a -> Maybe (Tree (a, Int))
bstRemove Leaf num = Nothing
bstRemove (Node (key, val) Leaf Leaf ) =
      if a == key
        then Leaf
        else Nothing
bstRemove (Node (key, val) left right) num
          | a > key = bstRemove right
          | a < key = bstRemove left
          | a == key = n
          where n = findInorder (Node (key, val) left right)--}




{--instance Show a => Show (Tree a) where
    show mytree = "Node " ++ show val ++ "\n" ++ "  " ++ show left ++ "\n" ++ "  " ++ show right
--}
