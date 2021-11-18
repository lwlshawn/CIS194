





{-
Monoids:

class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty  

satisfies the following rules:
mempty `mappend` x = x /existence of identity element,
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z) /associative

rules are NOT enforced by Haskell


you have a datatype Ordering, with LT EQ GT as its type constructors.
then you can make this an instance of Monoid, by defining the following
binary operator `mappend` as


LT `mappend` _ = LT
EQ `mappend` y = y
GT `mappend` _ = GT


Section: Maybe the Monoid

instance Monoid a => Monoid (Maybe a) where 
    ...

so you treat Maybe (a) as a monoid, if a is also a Monoid.



examples are also giving some appreciation for how newtype is useful
the way its used, we often take an existing type, and give it an "alias", 
so as to treat it as a "parallel type" similar to the old type, but now
we can use it and instance typeclasses in a way we want to.


if i do newtype Mytype = Mytype Int
is MyType an instance of all the things Int is an instance of?..no.
-}