{-# LANGUAGE GeneralizedNewtypeDeriving, MultiWayIf #-}

module Risk where

import Control.Monad.Random
import Control.Applicative
import Data.List
import Data.Monoid

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

--Ex2: implement a single battle
-- takes in an Int, produces an array of that many die rolls wrapped in Rand StdGen
rollN :: Int -> Rand StdGen [DieValue]
rollN n
    | n <= 0 = return []
    | n == 1 = (:[]) <$> die
    | otherwise = (:) <$> die <*> rollN (n-1)

-- sorts the rolls in decreasing order
sortedRollN :: Int -> Rand StdGen [DieValue]
sortedRollN n = reverse <$> sort <$> rollN n

-- converts a pair (att roll, def roll) to (-1, 0) or (0, -1) in Sum monoid
f :: (DieValue, DieValue) -> (Sum Int, Sum Int) 
f (a,b) 
    | a > b = (Sum 0, Sum (-1))
    | otherwise = (Sum (-1), Sum 0)

-- gives the result of a battle, in terms of how many casualties each side sustains
results :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen (Sum Int, Sum Int)
results a d = mconcat <$> ((fmap f) <$> (zip <$> a <*> d))

-- updates battlefield based on result of the battle.
battle :: Battlefield -> Rand StdGen Battlefield
battle Battlefield {attackers = a, defenders = d} =
  results (sortedRollN (min 3 (a - 1))) (sortedRollN (min 2 d)) >>= \(Sum n, Sum m) -> 
    return (Battlefield (a + n) (d + m))


--Ex3: implement invade, continuous battles until victor apparent
invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield {attackers = a, defenders = d}) 
    | a < 2 || d == 0 = return b
    | otherwise = battle b >>= invade


--Examples
b1 :: Battlefield
b1 = Battlefield 5 10

b2 :: Battlefield
b2 = Battlefield 10 10

-- evalRandIO $ invade b1 


--Ex4: Displaying the attacker win prob from a 1000 invasion simulation, given a battlefield

--Given a post invasion battlefield, return 1.0 if att wins, 0.0 if def wins.
winner :: Rand StdGen Battlefield -> Rand StdGen Double
winner b = b >>= \(Battlefield {defenders = d}) -> if
  | d == 0 -> return 1.0
  | otherwise -> return 0.0


--Given battlefield and an posint, produce list of battle outcomes wrapped in Rand StdGen
listOutcomes :: Battlefield -> Int -> Rand StdGen [Double]
listOutcomes b 1 = (:[]) <$> (winner $ invade b)
listOutcomes b n = (:) <$> (winner $ invade b) <*> listOutcomes b (n-1)


--Takes number of wins /1000
successProb :: Battlefield -> Rand StdGen Double
successProb b = (/1000) <$> (sum <$> (listOutcomes b 1000))









