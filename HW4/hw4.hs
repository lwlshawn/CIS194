--Ex1: Idiomatic corrections
fun1 :: [Integer] -> Integer
fun1 = foldr (*) 1 . map (\x -> x - 2) . filter even


-- this is currently wrong; i fail to sum the first element
-- its also more verbose then the original solution.
fun2 :: Integer -> Integer
fun2 n = n + fun2' n

fun2' :: Integer -> Integer
fun2' = sum . takeWhile (\x -> x /= 0) . iterate f where
    f n
        | n == 1 = 0
        | even n = n `div` 2
        | otherwise = 3 * n + 1

--Ex2: Fold a tree
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- could not think of a more idiomatic way to do this aside from this
-- extensive pattern checking owing to the structure of the given 
-- datatype.
insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node h Leaf m Leaf) = Node (h+1) (insert a Leaf) m Leaf
insert a (Node h Leaf m rt@(Node _ _ _ _)) = Node h (insert a Leaf) m rt
insert a (Node h lt@(Node _ _ _ _) m Leaf) = Node h lt m (insert a Leaf)
insert a (Node h lt@(Node hl _ _ _) m rt@(Node hr _ _ _))
    | hl < hr = Node h' lt' m rt
    | otherwise = Node h'' lt m rt'
    where
        lt'@(Node hl' _ _ _) = insert a lt
        rt'@(Node hr' _ _ _) = insert a rt
        h' = (max hl' hr) + 1
        h'' = (max hl hr') + 1



--Ex3: Practice with folds
-- returns True if there are odd number of Trues, False otherwise
xor :: [Bool] -> Bool
xor = (==1) . (`mod`2) . foldr (+) 0 . map(\x -> 1) . filter (\x -> x)

-- implement map using a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- Optional: implement foldl using foldr
-- skipped


--Ex4:
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) (filter (not . (`elem`xs)) [1..n]) where
    xs = map (\(x,y) -> 2 * x * y + x + y) (cartProd [1..n] [1..n])











