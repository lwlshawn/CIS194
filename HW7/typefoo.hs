class Tofu t where
    tofu :: j a -> t a j
-- so t is a type constructor, that takes in a and j. so (t a j) has kind *
-- (j a) also has kind *, so we infer that a has kind *, j has kind * -> *
{-
To summarise the kinds in the above example

a: *
j: * -> *
t: * -> (* -> *) -> *

Now we create a datatype Frank that has kind * -> (* -> *) -> *

This is the interesting part; despite the fact that Frank has that kind,
the data constructor does not require two different arguments! It only
requires one argument comprising two types. The two types must have kinds

(* -> *) and *.

-}


-- data Frank a b = Frank (b a)
data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu x = Frank x -- Frank is a type constructor, as well as a data type.
-- Here we are invoking the type constructor that takes in a single argument
-- This single argument should have type (b a)

-- Haskell infers that we are parameterising Frank with a and b appropriately 
-- on the left. 

--ex2
data Barry t k p = Barry {yabba :: p, dabba :: t k}
-- Make Barry an instance of fuctor, which wants types of kind * -> *
-- Barry takes in three parameters t k p. p should have kind *,
-- t has kind * -> *, and k has kind *, so Barry has kind
-- (* -> *) -> * -> * -> *

-- so before reading on, we likely have to pre-emptively parameterise
-- Barry with two types before doing instance of functor.


-- Reference:
{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}


_}

