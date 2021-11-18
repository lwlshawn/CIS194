{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import           Data.Maybe
import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
-- record syntax: recall that runParser is not just the "name" of the field
-- runParser is a function, that takes in a Parser, and pulls out the field.


-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

-- fmap :: (a -> b) -> Parser a -> Parser b (specialised to Parser)
instance Functor Parser where
    fmap f (Parser a) = Parser (\str -> (first f) <$> (a str))


{-
fmap f (Parser a) = Parser (\str -> pure (first f) <*> (a str))
the issue is that (a str) is inside a Maybe, but first f is not.
fmap f (Parser a) = Parser (\str -> first f (a str)) which is why this fails.
Here we utilise the fact that Maybe IS an applicative functor


pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b
here f is Parser, and its "contents" is a function
so <*> takes on its left, a Parser, and a function, to transform other functions.
instance Applicative Parser where
    pure a = Parser (\x -> Just(a,x))
    p1@(Parser {runParser = f1}) <*> p2@(Parser {runParser = f2}) 
        = Parser $ (helper f2) . fromJust . f1 

helper :: (String -> Maybe (a, String)) -> (a -> b, String) -> Maybe (b, String)
helper pf (f, str) = pure (first f) <*> (pf str)
-}

-- rewrote to make it total
instance Applicative Parser where
    pure a = Parser (\x -> Just (a,x))
    p1@(Parser {runParser = f1}) <*> p2@(Parser {runParser = f2})
        = Parser $ (helper1 f2) . f1

helper1 :: (String -> Maybe (a, String)) -> (Maybe (a->b, String)) -> Maybe (b, String)
helper1 _ Nothing = Nothing -- f1 fails
helper1 f2 (Just (f, str)) = (first f) <$> (f2 str)


-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
abParser :: Parser (Char, Char)
-- abParser = pure (,) <*> (char 'a') <*> (char 'b')
abParser = (,) <$> (char 'a') <*> (char 'b')
-- should be functionally equivalent


abParser_ :: Parser ()
abParser_ = pure (\x y -> ()) <*> (char 'a') <*> (char 'b')


intPair :: Parser [Integer]
--intPair = liftA3 (\a b c -> a : c : []) posInt (char ' ') posInt
intPair = (\a b c -> a : c : []) <$> posInt <*> (char ' ') <*> posInt


-- empty :: f a
-- (<|>) :: f a -> f a -> f a
instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    p1@(Parser {runParser = f1}) <|> p2@(Parser {runParser = f2})
        = Parser (\x -> (f1 x) <|> (f2 x))

-- subtlety is that you have to map f first; because
-- you can't do (Parser Int) <|> (Parser Char)
intOrUppercase :: Parser ()
intOrUppercase = (f <$> posInt) <|> (f <$> satisfy (isUpper)) where
    f = (\_ -> ())

