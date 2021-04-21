{- ##################################
   Using list comprehension instead of recursion 
   ################################## -}

module Prog3 where
import Data.Char

{- takes a list of ints, and returns a list of pairs, such that the first element in each pair 
is in the original number and the second element is the original number tripled
-}
tripleAll :: [Int] -> [(Int, Int)]
tripleAll x = map(\x->(x,x*3)) x

{- Function flip takes a list of pairs, and returns a list of pairs,with the pairs flipped -}
flip' :: [(Int, Int)] -> [(Int, Int)]
flip' x = map(\(x,y)->(y,x)) x

{- returns the sum of the lastnnumbers in the list,where n is the first argument to the function. -}
sumLastPart :: Int -> [Int] -> Int
sumLastPart x y = sum (drop (length y - x) y)

{-returns the product of the interior items in the list, that is, everything except the first and last item.-}
middleProduct :: [Int] -> Int
middleProduct x = sum(tail(init(x)))

{-init'that has identical behavior to the initfunction -}
init' :: [Int] -> [Int]
init' x = reverse(tail(reverse (x)))

{-generates a list of integer triples-}
triads :: Int -> [(Int,Int,Int)]
triads n = [(x*x,y*y,z*z) | x <- [0..n] , y <- [0..n] , z <- [0..n]]

{-takes a string and and an integernandforms a string of lengthnby putting spaces at the front of the string-}
pushRight :: String -> Int -> String
pushRight s n = replicate (n - length s) ' ' ++ s

{-lowercases the first characterin a string.-}
lowerFirstCharacter :: String -> String
lowerFirstCharacter s =   toLower (head s) : tail s

{-returns the ith item of the list, where thefirst item is index 1.-}
elemAt :: Int -> [Int] -> Int
elemAt n l = l !! (n-1)

{-returns the second word in a stringthat is composed of exactly three words-}
middleWord :: String -> String
middleWord s = words s !! 1
