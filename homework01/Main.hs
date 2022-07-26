module Main where

main :: IO ()
main = print (hanoi4 4 "a" "b" "c" "d")

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 0 = toDigitsRev (- x)
  | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
  | otherwise = []

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x : y : xs) = x : 2 * y : doubleEveryOther xs

digitSum :: Integer -> Integer
digitSum 0 = 0
digitSum n
  | n < 0 = digitSum (- n)
  | n < 10 = n
  | otherwise = n `mod` 10 + digitSum (n `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . digitSum) 0

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigitsRev) x `mod` 10 == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src dst aux = [(src, dst)]
hanoi n src dst aux = hanoi (n -1) src aux dst ++ [(src, dst)] ++ hanoi (n -1) aux dst src

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 src dst aux1 aux2 = []
hanoi4 1 src dst aux1 aux2 = [(src, dst)]
hanoi4 n src dst aux1 aux2 = hanoi4 (n -2) src aux1 aux2 dst ++ [(src, aux2), (src, dst), (aux2, dst)] ++ hanoi4 (n -2) aux1 dst src aux2
