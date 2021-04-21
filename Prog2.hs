{- ##################################
   Haskell Functions 
   ################################## -}

module Prog2 where
--A function twoSame that returns True if at least two of the three arguments are equal
twoSame :: Integer -> Integer -> Integer -> Bool
twoSame  x y z
  | x == y || x == z = True
  | y == z || y == z = True
  | x == y && y == z = True
  | otherwise = False


--A function sum' that computes the sum of all numbers from 1 to n (Recursion)
sum' :: Integer -> Integer
sum' 0 = 0
sum' n = n + sum' (n - 1)

-- A function abssum' that computes the sum of all numbers from -n to n (Recursion)
abssum' :: Integer -> Integer
abssum' 0 = 0
abssum' n = (n + (abssum' (n - 1)) - n - abssum' (n - 1))


--This redefines the built-in infix || operator. The new function or' is convereted to prefix notation.
or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True


--A function integerSqrt that returns the integer square root of a positive integer n.
integerSqrt :: Integer -> Integer
integerSqrt n = floor(sqrt(fromIntegral n))


--A function exponent' that recursively computes the result of raising some base number, b,to some exponent
exponent' :: Integer -> Integer -> Integer
exponent' n 0 = 1
exponent' m n  =  m * exponent' m (n - 1)


--A function swap that swaps the characters in a quintuple (5-tuple)
swap :: (Char, Char, Char, Char, Char) -> (Char, Char, Char, Char, Char)
swap (a,b,c,d,e) = (e,d,c,b,a)


--negateTwoDigits takes a list of integers andreturn a list of integers with all of the two-digit integers negated.
negateTwoDigits :: [Integer] -> [Integer]
negateTwoDigits x = [x * (-1) | x <- x]


--A function matches that uses list comprehension to pick out all instances of an integer nfrom a list.
matches :: Integer -> [Integer] -> [Integer]
matches key table = [ v | v <-table, key == v ]

--matches function to write a function element that uses listcomprehension to return True if an element is a member of a list
element :: Integer -> [Integer] -> Bool
element x y 
  | null (matches x y) = False
  | otherwise = True



