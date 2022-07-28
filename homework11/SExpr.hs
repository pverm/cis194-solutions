{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

main :: IO ()
main = do
  printParser (zeroOrMore (satisfy isUpper)) "(zeroOrMore (satisfy isUpper))" "abcdeFGh"
  printParser (oneOrMore (satisfy isUpper)) "(oneOrMore (satisfy isUpper))" "abcdeFGh"
  putStrLn ""
  printIdentParser "foobar baz"
  printIdentParser "foo33fA"
  printIdentParser "2bad"
  printIdentParser ""
  putStrLn ""
  printSExprParser "5"
  printSExprParser "foo3"
  printSExprParser "(bar (foo) 3 5 874)"
  printSExprParser "(((lambda x (lambda y (plus x y))) 3) 5)"
  printSExprParser "( lots of ( spaces in ) this ( one ) )"

printParser :: Show a => Parser a -> String -> String -> IO ()
printParser p ps e = putStrLn $ "runParser " ++ ps ++ " \"" ++ e ++ "\" == " ++ show (runParser p e)

printIdentParser :: String -> IO ()
printIdentParser = printParser ident "ident"

printSExprParser :: String -> IO ()
printSExprParser = printParser parseSExpr "parseSExpr"

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Show)

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

parseComb :: Parser [SExpr]
parseComb = char '(' *> zeroOrMore parseSExpr <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (A <$> parseAtom <|> Comb <$> parseComb) <* spaces