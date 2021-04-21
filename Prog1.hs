
module Prog1 where

-- isTwoDigitPositve is a 2 digit postive number 
isTwoDigitPositive :: Integer -> Bool
isTwoDigitPositive b1 = b1 > 9 && b1 < 100 && mod b1 2 == 0

test=isTwoDigitPositive(8)
test1=isTwoDigitPositive(12)
test2=isTwoDigitPositive(33)
test3=isTwoDigitPositive(100)

-- returns whether the second integer divides evenly into thefirst integer
dividesEvenly :: Integer -> Integer -> Bool
dividesEvenly b1 b2 = mod b1 b2 == 0

--returns the 2nd greatest of three given int arguments.
middle :: Integer -> Integer -> Integer -> Integer
middle b1 b2 b3 = 
    if (b1 <= b2 && b2 <= b3) || (b3 <= b2 && b2 <= b1) then b2
    else if (b2 <= b1 && b1 <= b3) || (b3 <= b1 && b1 <= b2) then b1
    else b3


--computes the NAND gate of two boolean arguments.
nand :: Bool -> Bool -> Bool
nand b1 b2 = if (b1 == True) && (b2 == True) then False else True

--converts a temperature in Fahrenheit into Celcius 
f2c :: Float -> Float
f2c c = (c - 32) * (5/9)

--floorDecimal that calculates the floor of a float, but returns it as a float ratherthan an integer.
--letterGrade that returns the equivalent letter grade for a given numericalinteger grade
letterGrade :: Integer -> String
letterGrade  l
    | l > 100 = "Invalid Letter Grade"   
    | l >= 93 && l <= 100 = "A"
    | l >= 90 && l <= 92 = "A-"
    | l >= 87 && l <= 89 = "B+"
    | l >= 83 && l <= 86 = "B"
    | l >= 80 && l <= 82 = "B"
    | l >= 77 && l <= 79 = "C+"
    | l >= 73 && l <= 76 = "C"
    | l >= 70 && l <= 72 = "C-"
    | l >= 67 && l <= 69 = "D+" 
    | l >= 63 && l <= 66 = "D"
    | l >= 60 && l <= 62 = "D-"
    | l <= 59 = "F"

--averageThree to return the average of three integers
averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = fromIntegral(a + b + c) / 3


--returns how many of three integer inputs are belowits average value

howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
howManyBelowAverage  a b c
  | (a < round (averageThree a b c) && b < round (averageThree a b c) && c < round (averageThree a b c)) = 3
  | (a < round (averageThree a b c) && b < round (averageThree a b c) && c > round (averageThree a b c)) = 2
  | (a < round (averageThree a b c) && b > round (averageThree a b c) && c < round (averageThree a b c)) = 2
  | (a > round (averageThree a b c) && b < round (averageThree a b c) && c < round (averageThree a b c)) = 2
  | (a < round (averageThree a b c) && b > round (averageThree a b c) && c > round (averageThree a b c)) = 1
  | (a > round (averageThree a b c) && b < round (averageThree a b c) && c > round (averageThree a b c)) = 1
  | (a > round (averageThree a b c) && b > round (averageThree a b c) && c < round (averageThree a b c)) = 1
  | otherwise = 0
