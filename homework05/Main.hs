{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import qualified Data.Map as M
import ExprT
import Parser
import StackVM

main :: IO ()
--main = print (testInteger, testBool, testMM, testSat)
--main = print testMP
main = print (withVars [("x", 6)] (add (lit 3) (var "x")))

-- Ex 1
eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add e1 e2) = (+) (eval e1) (eval e2)
eval (ExprT.Mul e1 e2) = (*) (eval e1) (eval e2)

-- Ex 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-- Ex 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Ex 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (<) 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit a = Mod7 (mod a 7)
  add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
  mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-- Ex 5
instance Expr Program where
  lit a = [StackVM.PushI a]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

testMP :: Maybe Program
testMP = compile "(3 * -4) + 5"

-- Ex 6
class HasVars a where
  var :: String -> a

data VarExprT
  = VarLit Integer
  | VarAdd VarExprT VarExprT
  | VarMul VarExprT VarExprT
  | Var String
  deriving (Eq, Show)

instance Expr VarExprT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var key = M.lookup key

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n _ = Just n
  add a b key = do
    n <- a key
    m <- b key
    return (n + m)
  mul a b key = do
    n <- a key
    m <- b key
    return (n * m)

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars bindings exp = exp (M.fromList bindings)