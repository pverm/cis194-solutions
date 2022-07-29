{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)

main :: IO ()
main = do
  let bf = Battlefield 12 10
  prob <- evalRandIO (successProb bf)
  print prob

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

-- Ex 2
getRolls :: Army -> Rand StdGen [DieValue]
getRolls n = replicateM n die

getMaxAttacker :: Army -> Army
getMaxAttacker n = if n >= 4 then 3 else n -1

getMaxDefender :: Army -> Army
getMaxDefender n = if n >= 2 then 2 else n

getMaxTroops :: Battlefield -> (Army, Army)
getMaxTroops (Battlefield att def) = (getMaxAttacker att, getMaxDefender def)

fight :: ([DieValue], [DieValue]) -> (Army, Army) -> (Army, Army)
fight ([], _) armies = armies
fight (_, []) armies = armies
fight (dA : dsA, dB : dsB) (att, def)
  | dA > dB = fight (dsA, dsB) (att, def -1)
  | otherwise = fight (dsA, dsB) (att -1, def)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) =
  do
    attRolls <- getRolls att
    defRolls <- getRolls def
    let (newAtt, newDef) = fight (reverse . sort $ attRolls, reverse . sort $ defRolls) (att, def)
    return (Battlefield newAtt newDef)

-- Ex 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield att def)
  | att > 1 && def > 0 = battle bf >>= invade
  | otherwise = return bf

-- Ex 4
judge :: [Battlefield] -> Double
judge bfs = fromIntegral (length wins) / fromIntegral (length bfs)
  where
    wins = filter (\bf -> defenders bf <= 0) bfs

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 (invade bf)
  return (judge results)