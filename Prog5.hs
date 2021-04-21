
module Prog5 where

{- 1. a function reverse' that reverses a list with a case expression -}
reverse' :: [a] -> [a]
reverse' fst = case fst of
  [] -> []
  (x:xs) -> reverse' xs ++ [x]

-- type TimeStamp = .. -- finish this type synonym definition

type TimeStamp = (Int, Int, Int)

{- 2. a function isShorter that takes two time stamps and returns 1 if the first is shorter -}
isShorter :: TimeStamp -> TimeStamp -> Int
isShorter (a,b,c) (d,e,f)
  | a < d = 1
  | a > d = -1
  | a == d && b < e = 1
  | a == d && b > e = -1
  | a == b && b == e && c < f = 1
  | a == b && b == e && c > f = -1
  | otherwise = 0

{- 3.totalSeconds that returns the number of total seconds that a video clip spans-}
totalSeconds :: TimeStamp -> Int
totalSeconds (a,b,c) = (a*60*60) + (b*60) + c

{-4. isValid that returns whether a time stamp is valid-}
isValid :: TimeStamp -> Bool
isValid (a,b,c)
  | a >= 0 && (b >= 0 && b < 60) && (c >= 0 && c < 60) = True
  | otherwise = False

{-5. time2Str that returns a string representation of timestamp in the form HH:MM:SS.-}
time2Str :: TimeStamp -> String
time2Str (h,m,s) = doubleString(show h) ++ ":" ++ doubleString(show m) ++ ":" ++ doubleString(show s)

{-Constructor-}
data Set345 = NonEmptySet [Int]
            | EmptySet
      deriving Show

{-6. member that checks whether the given item is present in the given set-}
member :: Int -> Set345 -> Bool
member x (NonEmptySet xs) = elem x xs

{-7. function size that returns the number of elements in a given set-}
size :: Set345 -> Int
size (NonEmptySet xs) =  length xs

{-8.ins that inserts the given item into a set-}
ins :: Int -> Set345 -> Set345
ins x (NonEmptySet xs)
  | member x (NonEmptySet xs) = NonEmptySet xs
  | otherwise = NonEmptySet (x:xs)

{-9. safeLast that behaves similarly to the built-in last function -}
safeLast :: [Int] -> Maybe Int
safeLast xs 
  | length xs == 0 = Nothing
  | otherwise = Just (xs!!((length xs)- 1))

{-10.safeCount that takes an item to search for and a list of numbers, and returns Nothing if the list is empty-}
safeCount :: Int -> [Int] -> Maybe Int
safeCount x xs
  | length xs == 0 = Nothing
  | otherwise = Just (doCount x xs)

--helper function 
doCount :: Int -> [Int] -> Int
doCount x (y:ys)
  | ys == [] && x == y = 1
  | ys == [] = 0
  | x == y = 1 + (doCount x ys)
  | otherwise =  (doCount x ys)


