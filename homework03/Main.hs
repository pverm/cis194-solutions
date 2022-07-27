module Main where

main :: IO ()
main = putStrLn (histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9])

skipN :: Int -> [a] -> [a]
skipN n = map snd . filter (\(x, y) -> x `mod` n == 0) . zip [1 ..]

skips :: [a] -> [[a]]
skips xs = zipWith skipN [1 ..] (replicate (length xs) xs)

localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : xs)
  | x < y && y > z = y : localMaxima (z : xs)
  | otherwise = localMaxima (y : z : xs)
localMaxima _ = []

count :: [Int] -> Int -> Int
count xs n = length (filter (== n) xs)

line :: [Int] -> Int -> String
line xs maxNum = [if x >= maxNum then '*' else ' ' | x <- xs]

histogram :: [Int] -> String
histogram xs =
  let counts = map (count xs) [0 .. 9]
      maxCount = maximum counts
   in unlines (map (line counts) [maxCount, maxCount -1 .. 1]) ++ "==========\n0123456789\n"
