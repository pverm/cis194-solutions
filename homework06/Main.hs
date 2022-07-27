{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main where

main :: IO ()
--main = print $ take 20 fibs2
--main = print $ streamMap (+ 10) (streamRepeat 50)
--main = print $ streamFromSeed (+ 6) 0
--main = print $ interleaveStreams (streamRepeat 5) (streamRepeat 10)
--main = print ruler
--main = print ((x ^ 2 + x + 3) * (x - 5))
--main = print fibs3
main = print $ fib4 1000

fib :: Integer -> Integer
fib n
  | n <= 1 = n
  | n > 1 = fib (n -1) + fib (n -2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Ex 2
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)

-- Ex 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

-- Ex 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xr) = Cons (f x) (streamMap f xr)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f (f seed)

-- Ex 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xr) ys = Cons x (interleaveStreams ys xr)

ruler :: Stream Integer
ruler = ruler' 0
  where
    ruler' y = interleaveStreams (streamRepeat y) (ruler' (y + 1))

-- Ex 6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  (+) (Cons x xr) (Cons y yr) = Cons (x + y) (xr + yr)
  (*) (Cons x xr) (Cons y yr) = Cons (x * y) (streamMap (* x) yr + (xr * Cons y yr))
  fromInteger n = Cons n (streamRepeat 0)
  negate (Cons x xr) = Cons (- x) (negate xr)

instance Fractional (Stream Integer) where
  (/) (Cons a as) (Cons b bs) = q
    where
      q = Cons (a `div` b) (streamMap (`div` b) (as - q * bs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

-- Ex 7
data Matrix = Matrix Integer Integer Integer Integer deriving (Show)

instance Num Matrix where
  (*) (Matrix a1 b1 c1 d1) (Matrix a2 b2 c2 d2) =
    let a3 = a1 * a2 + b1 * c2
        b3 = a1 * b2 + b1 * d2
        c3 = c1 * a2 + d1 * c2
        d3 = c1 * b2 + d1 * d2
     in Matrix a3 b3 c3 d3

matrixFst :: Matrix -> Integer
matrixFst (Matrix a _ _ _) = a

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = matrixFst $ Matrix 1 1 1 0 ^ (n - 1)
