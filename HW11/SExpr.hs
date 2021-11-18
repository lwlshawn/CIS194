{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> pure []


oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (++) <$> ((:[]) <$> p) <*> (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------
-- parse a consecutive list of zero or more whitespace characters
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)


--parse an identifier, which is an alphabetic character, followed by
--zero or more alphanumeric characters

-- high level idea is to run satisfy isAlpha first,
-- and then run zeroOrMore (satisfy isAlphaNum)
ident :: Parser String
ident = (++) <$> ((:[]) <$> (satisfy isAlpha)) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


{-
Textually, S-expressions can optionally begin and end with any
number of spaces; after throwing away leading and trailing spaces they
consist of either an atom, or an open parenthesis followed by one or
more S-expressions followed by a close parenthesis.
-}

parseAtom :: Parser SExpr
parseAtom = A <$> I <$> ident <|> A <$> N <$> posInt

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (parseAtom <|> ( (char '(') *> (Comb <$> oneOrMore parseSExpr) <* (char ')') )) <* spaces
-- not sure if this was intended, but this solution results in (foo) being interpreted as a singleton list
-- instead of an atom. Seems like a reasonable way to implement it.







