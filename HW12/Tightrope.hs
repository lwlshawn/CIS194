module Tightrope where

import Data.Maybe

type Birds = Int 
type Pole = (Birds,Birds)


landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing 


routine :: Maybe Pole  
-- routine = do  
--     start <- return (0,0)  
--     first <- landLeft 2 start  
--     Nothing  //this line equivalent to >>
--     second <- landRight 2 first  
--     landLeft 1 second 

-- routine = return (0,0) >>= (\x ->
--           landLeft 2 x >>= (\_ ->
--           Nothing >>= (\y ->
--           landRight 2 y >>= (\z ->
--           landLeft 1 z))))

routine = return (0,0) >>= (\x ->
          landLeft 2 x >> 
          Nothing >>= (\y ->
          landRight 2 y >>= (\z ->
          landLeft 1 z)))

-- ">>" means ignore my left argument, and just give the right argument
-- ">>" is equivalent to ">>= \_ -> x" where x is the argument you want

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- routine = return (0,0) >>= landLeft 2 >>= banana >>= landRight 2
-- routine = return (0,0) >>= landLeft 2 >> Nothing >>= landRight 2

-- ['a','b','a','b'] >>= \ch -> return (n, ch)
-- the intuition is clear but i don't understand, how Haskell is understanding this

-- xs >>= f = concat (map f xs); what happens is that f is a function, that takes a value,
-- and produces a value in a context. So in this case, f takes a value, and produces a list
-- of that same type as the initial value.

-- map does this to each element, so you get a list of lists, and concat flattens this back down.
-- here, we map the function \ch -> [(n,ch)] over the list ['a','b','a','b']

-- but n is a LIST. so what I expect the result to be instead is
-- [([1,2],'a'), ([1,2],'b'), ([1,2],a), ([1,2],b)]

-- ah but the function \n -> ['a','b'] does not bind n to a list. Instead, it binds n
-- to 1, and then binds n to 2. How on earth does haskell correctly process this though,
-- i don't understand how the nondeterminism propagates in such a fashion. 

-- somehow haskell is "remembering" the fact that n was bound to multiple values, and 
-- correctly recalls all the values to which n came from.



-- ma >>= \a -> sequence mas >>= \as -> return (a:as)
-- (ma >>= \a -> sequence mas) >>= \as -> return (a:as)
-- we take the first monad in the list [m a] and bind it to ma
-- and we extract a from it, and call sequence mas (so we extract a, and do nothing with it)
-- sequence mas returns a monad of type m [a] so now we pull [a] out and bind it to \as
-- and concatenate a with as. Finally, we wrap this concatenated list in return to put it
-- back into context.

-- makes sense, sequence just "extracts" the contents of the monads out of their contexts
-- to form a list of the contents, and then rewraps that into the monad context.







