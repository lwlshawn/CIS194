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
{-
random :: RandomGen g => g -> (a, g)
this is a function required if you want to instance the typeclass Random
so you take a g, and return a tuple (a,g) where g is an instance of RandomGen

the high level is that g is a random generator, and you return a tuple of 
a value a, and the "next" generator.

My interpretation is that throughout we already have parameterised the generator
in that Rand StdGen is a monad, that is making use of the generator "StdGen".

Why this is confusing is that if i want to use runRand or evalRand, i still need
to provide a "initial generator", haven't we already specified that the "initial generator"
be StdGen?
-}

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
rollN 1 = (:[]) <$> die
rollN n = (:) <$> die <*> rollN (n-1)


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


--Ex4: 
winner :: Rand StdGen Battlefield -> Rand StdGen Double
winner b = b >>= \(Battlefield {defenders = d}) -> if
  | d == 0 -> return 1.0
  | otherwise -> return 0.0


listBattles :: Battlefield -> Int -> Rand StdGen [Double]
listBattles b 1 = (:[]) <$> (winner $ invade b)
listBattles b n = (:) <$> (winner $ invade b) <*> listBattles b (n-1)


successProb :: Battlefield -> Rand StdGen Double
successProb b = (/1000) <$> (sum <$> (listBattles b 1000))





--random notes below
threeInts :: Rand StdGen (Int, Int, Int)
threeInts = getRandom >>= \i1 ->
            getRandom >>= \i2 ->
            getRandom >>= \i3 ->
            return (i1, i2, i3)

{-
why does this work? Rand StdGen is supposed to be a monad, according to his notes.
lets keep things simple. For now assume there are max attackers and max defenders
how do I make use of Random DieValue and Rand StdGen Dievalue?

getRandom documentation says it returns a computation, that returns a random value,
together with a new generator. Lets try to understand the threeInts code with this in mind

getRandom >>= (\i1 -> getRandom >>= \i2 -> getRandom >>= \i3 -> return (i1, i2, i3))

>>= uses the final generator of the first computation as the generator of the second
so \i1 pulls out the "value" of the first computation from getRandom, and somehow
chaining in this manner will pass the new generator onwards.

on an abstract level, i suppose when i call getRandom, i get a monad that somehow
encapsulates both the value, and the generator that was used, to generate that value.

chaining it with >>= the value that is "taken in" by the next function, is the value
created by the previous function then we just call getRandom again; 

-}


{- 
Typically >>= is meant to take a mobit on the left, and on the right, a function that
takes a "normal value", and produces a mobit. so it has signature

(>>=) :: m a -> (a -> m b) -> m b
The high level idea is that it "extracts" a value in a context, to apply a function
that works with a value, to create a new value in a context.

Control.Monad.Random.Class -- class Monad m => MonadRandom m where
so the restraint is that a m has to be a monad, to be a MonadRandom

getRandom :: Random a => m a
what? so getRandom is just a value, it has a type constructor m of kind * -> *
and a type a of kind *, so long as a is an instance of Random.

ok so ignoring the details, getRandom is a method that has to be implemented
if you want your monad to be an instance of the class MonadRandom.

what getRandom does is it returns a computation together with a new generator.
-}







