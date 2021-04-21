{- ##################################
    only recursion and standard built-infunctions (and helper functions). 
    NO LIST COMPREHENSION ALLOWED 
   ################################## -}

module Prog4 where
import Data.Char

{- This function takes a list of ints, and returns a list of pairs
(the 1st element in each pair is in the original number, the 2nd is the original number tripled).-}
tripleAll :: [Int] -> [(Int,Int)]
tripleAll [] = []
tripleAll (x:xs) = (x, (3 * x)) : (tripleAll xs)

{-takes a list of pairs, and returns alist of pairs, with the pairs flipped 
(the first item becomes the second item,and vice versa) -}
flip' :: [(Int,Int)] -> [(Int,Int)]
flip' [] = []
flip' ((x,y) : xs) = (y, x) : (flip' xs)

{- returns the sum of the lastnnumbers inthe list, where n is the first argument to the function. -}
sumLastPart :: Int -> [Int] -> Int
sumLastPart x list = addProduct(drop x list)

{-returns the product of the interior itemsin the list (everything except the first and last item). -}
middleProduct :: [Int] -> Int
middleProduct list = addProduct(tail(reverse (tail (reverse list))))
addProduct :: [Int] -> Int
addProduct [] = 0 
addProduct (x:xs) = x + (addProduct(xs))

{-identical behavior to the init function-}
init' :: [Int] -> [Int]
init' (x:xs)
  | length xs <=1 = [x]
  | otherwise = x : init' xs 

{-Lowercases the first,third, fifth letter (and so on) of a string.-}
lowerOddLetters :: String -> String 
lowerOddLetters s 
  | length s < 2 = s 
  | otherwise = toLower ( head s) : head (tail s) : lowerOddLetters ( tail (tail s )) 

{-returns the ith item of the list, where the first item is index 1.-}
elemAt :: Int -> [Int] -> Int
elemAt x (y:ys)
  | x == 1 = y
  | otherwise = elemAt (x - 1) ys

ins' :: (String,Int) -> [(String,Int)] -> [(String,Int)]
ins' (x,y) [] = [(x,y)]
ins' x (y:ys)
    |snd x < snd y = x:y:ys 
    |otherwise = y:ins' x ys

{-insertion sort to sort a list of pairs-}
iSort' :: [(String,Int)] -> [(String,Int)]
iSort' [] = []
iSort' (x:xs) = ins' x (iSort' xs)
elemAtString :: Int -> [String] -> String
elemAtString x (s:ys) 
 | x == 0 = s
 | otherwise = elemAtString (x - 1) ys

{-returns the second word in a string that is composed of exactly three words. -}
middleWord :: String -> String
middleWord list = elemAtString 1 (words list)

{-Lowercases the first uppercase letter in a string-}

lowerLastLetter :: String -> String
lowerLastLetter (g:xs)
  | length xs == 0 = [(toLower g)]
  | otherwise = g : lowerLastLetter(xs)

lowerFirstLetter :: String -> String
lowerFirstLetter list = reverse (lowerLastLetter(reverse list))

