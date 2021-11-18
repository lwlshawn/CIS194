{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where
import Parser
import ExprT
import StackVM
import qualified Data.Map as M
import Data.Maybe

--Ex1 and Ex2
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)


evalStr :: String -> Maybe Integer
evalStr = evalStr' . (parseExp ExprT.Lit ExprT.Add ExprT.Mul) where
    evalStr' (Just x) = Just (eval x)
    evalStr' Nothing = Nothing


--Ex3: Perhaps more accurate would have been to do Expr a b to allow
-- lit b -> a instead of hardcoding an Integer. This appears to be
-- the intended solution from all his examples however.
class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a


instance Expr ExprT where
    lit = ExprT.Lit
    mul = ExprT.Mul
    add = ExprT.Add

--Ex4
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min


instance Expr Mod7 where
    lit = Mod7
    add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
    mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit a
        | a < 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


--Ex5: Goal is to allow arithmetic expressions to be interpreted as compiled programs
-- i.e. parseExp lit add mul "4+5" should give [PushI 4, PushI 5, Add]
-- Generalisation for Booleans does not appear intended
instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul


--Ex6
-- Recall that a class, is like a interface in Java. Things that implement this class
-- must fulfill a promise, that they have a function var, with type signature
-- String -> a.
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)


instance Expr VarExprT where
    lit = Calc.Lit
    add = Calc.Add
    mul = Calc.Mul


instance HasVars VarExprT where
    var = Var 


instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- type MyType = M.Map String Integer -> Maybe Integer

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = (\_ -> Just n) 

    add f1 _ m | isNothing (f1 m) = Nothing
    add _ f2 m | isNothing (f2 m) = Nothing
    add f1 f2 m = Just ((fromJust (f1 m)) + (fromJust (f2 m)))

    mul f1 _ m | isNothing (f1 m) = Nothing
    mul _ f2 m | isNothing (f2 m) = Nothing
    mul f1 f2 m = Just ((fromJust (f1 m)) * (fromJust (f2 m)))


withVars :: [(String, Integer)] 
         -> (M.Map String Integer -> Maybe Integer) 
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

-- withVars takes two arguments, a list, a function, and returns a Maybe Integer
-- so here the first argument is the list, and exp is a function; so he
-- converts vs to a hash map of mappings; and then feeds that into exp, which 
-- has type (M.Map String Integer -> Maybe Integer)

-- withVars [("x", 6), ("y",3)] $ mul (var "x") (add (var "y") (var "x"))
-- so here withVars converts the first list to a Map, then the second
-- argument, is an arithmetic expression. 

-- Here because of the signature of withVars, Haskell probably understands that
-- the correct implementation of lit add mul to use is the 
-- instance Expr (M.Map String Integer -> Maybe Integer) case

-- innermost add (var "y") (var "x") returns a function; that takes in a map,
-- ah here i get it. var "y" it understands that i'm calling the var method,
-- declared under instance HasVars (M.Map String Integer -> Maybe Integer)
-- so add feeds the correct map into this function

--finally the lit declaration works because the idea is that it doesn't matter
-- what is stored in the hashmap, because you don't need to lookup the value
-- you simply return the literal integer.





