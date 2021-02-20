{-
1)
  a- zy
  b- y
  c- \x.\y(a x)y b c
  d- (\y.(y y))
  e- infinite reduction
  f- \y.z
2) \m.\n.(plus n)Z
3)




-}

import Data.String


harmonic :: Int -> Double
harmonic 1 = 1
harmonic i = 1 / fromIntegral i + harmonic (i-1)

countQs :: String -> Int
countQs "" = 0
countQs ('q':t) = 1 + countQs (t)
countQs ('Q':t) = 1 + countQs (t)
countQs (h:t) = countQs(t)

mytake :: Int -> [String] -> [String]
mytake 0 s = []
mytake i [] = []
mytake 1 (h:t) = [h]
mytake i (h:t) =  [h] ++ mytake (i-1) t

mylast :: [Int] -> Int
mylast [] = 0
mylast [x] = x
mylast (h:t) = mylast t

range :: Int -> Int -> [Int]
range first last =
  if last < first
    then []
    else
      if last == first
        then [first]
        else  [first] ++ range (first + 1) last

run :: String -> Int
run "" = 0
run [x] = 1
run (h:t) =
  if [h] == head [t]
    then 1 + run t
    else 1


longestRun :: String -> Int
longestRun "" = 0
longestRun [x] = 1
longestRun s =
  if run s > a
    then run s
    else a
  where a = longestRun (tail s)
