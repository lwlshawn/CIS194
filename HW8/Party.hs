module Party where


import Employee
import Data.Monoid
import Data.Tree
import Data.List

--Ex1:
-- simply adds employee to the list and their funscore, assuming
-- they are not on the list, and none of their direct subordinates are in the list
glCons :: Employee -> GuestList -> GuestList 
glCons e@(Emp {empFun = n}) (GL gl acc) = GL (e : gl) (acc + n)


instance Monoid GuestList where
    mempty = GL [] 0
    -- (GL gl1 acc1) `mappend` (GL gl2 acc2) = GL (gl1 ++ gl2) (acc1 + acc2)

instance Semigroup GuestList where
    (GL gl1 acc1) <> (GL gl2 acc2) = GL (gl1 ++ gl2) (acc1 + acc2)

--why doesn't monoid allow me to impement <> but semigroup does?
--why do i have to implement semigroup, to instance monoid?


-- moreFun :: GuestList -> GuestList -> GuestList
-- moreFun gl1@(GL _ n1) gl2@(GL _ n2)
--     | n1 < n2 = gl1
--     | otherwise = gl2

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max


--Ex2: 

{-
second argument, named f, takes in folded left tree, 
curr node val, folded right tree to produce a single value
first argument to treeFold is the "default" value.

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)
-}

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node {rootLabel = a, subForest = []}) = f a []
treeFold f (Node {rootLabel = a, subForest = ls}) = f a (map (\subtree -> treeFold f subtree) ls)

{-
High level is like i want to map treeFold to the subForest list.
Now that i have a list [b], i want to convert that into a single value
so f should take in [b] and return a single value b
-}



{-
Ex3: Employee is the "boss", second argument is list of results for each subtree
under Boss. Each result is a pair of GuestLists, the first GuestList in the pair
is best possible list with the boss of that subtree, second is best possible guest list
without the boss of that subtree

if i want to take the "boss", none of this direct subordinates can be invited, so i just take
the value of the best possible subtrees that don't involve the subtrees boss.

if i dont take the "boss", i can take, or not his direct subordinate. So i simply take
the best possible subtree of the two available at each step.

the form i'm going to use is the first guestlist is with the boss, second is without
-}


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b@(Emp {empFun = n}) [] = (GL [b] n, GL [] 0)
nextLevel b ls =  (with, without) where
    with = glCons b $ mconcat $ map snd ls
    without = mconcat $ map (\(a,b) -> moreFun a b) ls


{-
Ex4: Take company hiearchy tree as input, outputs a fun-maximizing guest list
-}

maxFun :: Tree Employee -> GuestList
maxFun = max' . treeFold nextLevel

max' :: Ord a => (a,a) -> a
max' (a,b) = if a > b then a else b
-- assuming this is the intended strategy, then treeFold should have a different
-- type signature. it should be b -> (a -> [b] -> b) -> Tree a -> b



{-
so foldr has type Foldable t => (a -> b -> b) -> b -> t a -> b
this reads; it takes a function with two parameters a and b, that returns a value b
it takes a value b
it takes a type constructor t that is an instance of Foldable, with type parameter a
and it finally returns a value of b

The reason for the second argument, the value of b, is that this is like the 
"starting value" of the accumulator.

How does this differ in my tree context?

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
well actually the trees are guaranteed to be nonempty so the first part is unnecessary.

this gives treeFold :: (a -> [b] -> b) -> Tree a -> b
-}

getList :: GuestList -> [Employee]
getList (GL ls _) = ls

getFun :: GuestList -> Fun
getFun (GL _ f) = f

-- Ex5:

main :: IO ()
main = do
    s <- readFile "company.txt"
    let g = maxFun (read s)
    putStrLn $ "Total fun: " ++ (show $ getFun g)
    --mapM_ putStrLn $ sort $ map empName $ getList g
    putStrLn $ unlines $ sort $ map empName $ getList g --produces extra newline at end

-- why does putStrLn $ "Total fun: " ++ $ show $ getFun g fail?
-- ++ has higher precedence then $, so ++ tries to act first
-- and tries to take $ as an argument? If i had to guess.









