{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}
module JoinList where 

import Data.Monoid
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


--Ex1: Implementing (+++) and tag
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty l = l 
(+++) l Empty = l 
(+++) l1@(Single m1 _) l2@(Single m2 _) = Append (m1 <> m2) l1 l2
(+++) l1@(Single m1 _) l2@(Append m2 _ _) = Append (m1 <> m2) l1 l2
(+++) l1@(Append m1 _ _) l2@(Single m2 _) = Append (m1 <> m2) l1 l2
(+++) l1@(Append m1 _ _) l2@(Append m2 _ _) = Append (m1 <> m2) l1 l2 


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m
--tag is a function, that takes a JoinList m a provided m is a monoid, and
--it returns a Monoid m. annotation here refers to the nummeric value in the diagram.


--Ex2: 
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a

indexJ i _ | i < 0 = Nothing
indexJ _ Empty = Nothing
indexJ n (Single _ a) = if n == 0 then Just a else Nothing
indexJ n (Append _ (Single _ a) l) = if n == 0 then Just a else indexJ (n - 1) l
indexJ n (Append _ l1@(Append m _ _) l2) = if (Size n) < (size m) 
                                                then indexJ n l1 
                                                else indexJ (n - ((getSize . size) m)) l2


jl1 = Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b')
jl2 = Append (Size 3) (Single (Size 1) 'c') (Append (Size 2) (Single (Size 1) 'd') (Single (Size 1) 'e'))
jl3 = jl1 +++ jl2


--creates a list out of the 'a' contained in the leaf nodes
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)


--i omitted the cases with Append containing Empty as this generally
--seems like unimportant tedium
dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a

dropJ i l | i <= 0 = l
dropJ _ Empty = Empty
dropJ n (Append _ (Single _ a) l) = dropJ (n - 1) l
dropJ n (Append _ l1@(Append m _ _) l2) = if (Size n) < (size m)
                                                then dropJ n l1
                                                else dropJ (n - ((getSize  . size) m)) l2


takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ i l | i <= 0 = Empty
takeJ _ Empty = Empty
takeJ 1 l@(Single _ _) = l
takeJ n (Append _ l1@(Single _ a) l2) = l1 +++ (takeJ (n-1) l2)
takeJ n (Append _ l1@(Append m _ _) l2) = if (Size n) < (size m)
                                                then takeJ n l1
                                                else l1 +++ (takeJ (n - ((getSize  . size) m)) l2)


--Ex3: Rest is in Scrabble.hs
scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x


--Ex4: Each leaf in the tree represents a line, so size of a leaf is 1
--assumption here is that the string given to fromString is like a text,
--that we wish to convert to a buffer. 
instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList --combined string representation of "leaf" nodes

    --break into lines, convert to singles, then append
    fromString = foldr (\acc l -> acc +++ l) Empty . (map (\s -> (Single (scoreString s, 1) s))) . lines 

    line = indexJ

    --say that I find the line that i want. the new tree should just replace this leaf and leave everything else untouched
    replaceLine i _ tree | i < 0 = tree
    replaceLine _ _ Empty = Empty
    replaceLine n s l@(Single _ _) = if n == 0 then (Single (scoreString s, 1) s) else l --reached leaf, replace or leave untouched
    replaceLine n s (Append m l1 l2)
      | (Size n) < (size m) = Append m' l1' l2 --recurse on left tree
      | otherwise = Append m'' l1 l2' --otherwise recurse on right tree
      where
        l1' = replaceLine n s l1
        l2' = replaceLine (n - (numLines l1)) s l2
        m' = (tag l1') <> (tag l2)
        m'' = (tag l1) <> (tag l2')


    numLines = getSize . snd . tag

    value = getScore . fst . tag


createBuffer :: String -> (JoinList (Score, Size) String)
createBuffer = fromString --fromString cant be accessed from a different folder!





