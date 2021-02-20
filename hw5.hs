{------------------Definitions-----------------------}
data Stream a = Chunk a (Stream a)

data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving Show

{--
Question 1:

The function is not tail recursive.
This is the tail-call recursive form

def mystery (lst, startIndex):
  if len (lst) - 1 <= startIndex:
    return lst
  else:
    lst[startIndex], lst[startIndex + 1] = lst[startIndex + 1], lst[startIndex]
    return mystery (lst, startIndex + 2)

--}



{-------------------Functions----------------------}

streamToList :: Stream a -> [a]
streamToList (Chunk v next) = v : streamToList next

streamRepeat :: a -> Stream a
streamRepeat var = Chunk var (streamRepeat var)

instance Functor (Stream) where
  fmap f (Chunk x xs) = Chunk (f x)  (fmap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f num = Chunk num (streamFromSeed f (f num))



{--treeHelper :: Tree Integer -> Tree Integer 
treeHelper (Node val left right) = (Node val + 1 l r)


bigTree :: Tree Integer
bigTree = Node 0 (bigTree) (bigTree) --}

collatzFunc :: Integer -> Integer
collatzFunc 1 = 1
collatzFunc n =
  if (even n)
    then (div n 2)
    else (3 * n + 1)

collatz :: Integer -> Stream Integer
collatz num = streamFromSeed collatzFunc  num

collatzLength :: Integer -> Integer
collatzLength 1 = 0
collatzLength num = 1 + collatzLength h
    where h = (streamToList (collatz num)) !! 1

longestCollatz :: Integer -> Integer
longestCollatz 1 = 0
longestCollatz num =
                  if ofNum > ofLower
                    then num
                    else ofLower
                  where ofNum = collatzLength num
                        ofLower = longestCollatz(num-1)
