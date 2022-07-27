module Main where

import Data.List ((\\))

main :: IO ()
--main = print (foldTree "ABCDEFGHIJ")
--main = print (xor [True, False, False])
main = print (sieveSundaram 500)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insertNode :: a -> Tree a -> Tree a
insertNode a Leaf = Node 0 Leaf a Leaf
insertNode a (Node h left b right)
  | height left > height right = Node h left b (insertNode a right)
  | height left < height right = Node h (insertNode a left) b right
  | otherwise =
    let inserted = insertNode a left
     in Node (1 + height inserted) inserted b right

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

xor :: [Bool] -> Bool
xor = foldr xor' False
  where
    xor' True a = not a
    xor' False a = a

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x = foldr (flip f) x . reverse

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let combinations = cartProd [1 .. n] [1 .. n]
      p = (\(i, j) -> i + j + 2 * i * j <= n)
      f = (\(i, j) -> i + j + 2 * i * j)
   in map (\x -> 2 * x + 1) ([1 .. n] \\ (map f . filter p) combinations)