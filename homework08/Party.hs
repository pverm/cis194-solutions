module Party where

import Data.Monoid
import Data.Semigroup
import Data.Tree
import Employee

-- Ex 1
instance Semigroup GuestList where
  GL es1 f1 <> GL es2 f2 = GL (es1 ++ es2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) (GL es s) = GL (e : es) (s + f)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 > f2 = gl1
  | otherwise = gl2

-- Ex 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node l ts) = f l $ map (treeFold f) ts

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs emp xs = glCons emp (mconcat xs)

-- Ex 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss, withoutBoss)
  where
    withBoss = glCons boss $ mconcat (map snd gls)
    withoutBoss = mconcat $ map (uncurry moreFun) gls

-- Ex 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Ex 5
main :: IO ()
main = do
  input <- readFile "company.txt"
  let output = printGL . maxFun . read $ input
  putStrLn output

printGL :: GuestList -> String
printGL (GL gs n) = "Total fun: " ++ show n ++ "\nGuests:\n" ++ unlines (map printEmp gs)

printEmp :: Employee -> String
printEmp (Emp name f) = "Name: " ++ name ++ " (Fun: " ++ show f ++ ")"
