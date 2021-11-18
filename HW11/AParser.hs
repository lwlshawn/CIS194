module AParser (Parser, runParser, satisfy, char, posInt) where

import           Control.Applicative
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

inParser f = Parser . f . runParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap = inParser . fmap . fmap . first




-- here Parser has a function with argument s, and then it pattern matches
-- on the result of fp s; if fp s = Nothing (first function fails), it fails
-- o/w first function succeeds, and produces (f, rem str)

-- xp is the second parser; so what you want to do is to run the second parser
-- on s', and then map f over that result. makes sense.

-- notably case syntax is very useful for anonymous functions, since you
-- can't pattern match when the function has no name (is anonymous)


instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2
