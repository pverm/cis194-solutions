{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Ex 1
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single t _) = t
tag (Append t _ _) = t

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x `mappend` tag y) x y

-- Ex 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ _ Empty = Nothing
indexJ i (Single t a)
  | i == 0 = Just a
  | otherwise = Nothing
indexJ i (Append t jla jlb)
  | i < 0 || i > getSize (size t) = Nothing
  | i < size' = indexJ i jla
  | otherwise = indexJ (i - size') jlb
  where
    size' = getSize (size (tag jla))

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n j | n <= 0 = j
dropJ n (Append t jla jlb)
  | n >= getSize (size t) = Empty
  | n >= size' = dropJ (n - size') jlb
  | n > 0 = dropJ n jla +++ jlb
  where
    size' = getSize (size (tag jla))
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n j | n <= 0 = Empty
takeJ n jl@(Single _ _) = jl
takeJ n jl@(Append t jla jlb)
  | n >= getSize (size t) = jl
  | n >= size' = jla +++ takeJ (n - size') jlb
  | n > 0 = takeJ n jla
  where
    size' = getSize (size (tag jla))
takeJ _ _ = Empty

-- Ex 3
-- File Scrabble.hs
scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine s = Single (scoreString s) s

-- Ex 4
instance Buffer (JoinList (Score, Size) String) where
  toString jl = case jl of
    Empty -> ""
    Single _ s -> s
    Append _ jla jlb -> toString jla ++ toString jlb
  fromString = foldl (\jl t -> jl +++ Single (scoreString t, 1) t) Empty . lines
  line = indexJ
  replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n + 1) jl
  numLines = getSize . size . tag
  value = getScore . fst . tag

testStr :: String
testStr =
  unlines
    [ "This buffer is for notes you don't want to save, and for",
      "evaluation of steam valve coefficients.",
      "To load a different file, type the character L followed",
      "by the name of the file."
    ]

main :: IO ()
main = runEditor editor (fromString testStr :: (JoinList (Score, Size) String))