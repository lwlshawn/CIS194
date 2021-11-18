module Golf where
{-
Rules: Whitespace and type signatures dont count
Otherwise just count total number of characters
-}


-- Ex1: for nth item in list, create list of items with indices k*n
-- Done by converting original list to list including indices for items
skips :: [a] -> [[a]]
skips l = [h n x | (n,_) <- x] where x = zip [1..] l

-- Helper takes in n and list of tuples, returns a filtered list of items
h :: Int -> [(Int,a)] -> [a]
h n l = [x | (_,x) <- filter (\(t,_) -> mod t n == 0) l]


-- Ex2: item by item comparison and concatenation.
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:[]) = []
localMaxima (x:xs) = h' x xs

h' :: Integer -> [Integer] -> [Integer]
h' prev (c:[]) = []
h' prev (c:x:xs)
    | c > x && c > prev = c : h' c (x:xs)
    | otherwise = h' c (x:xs)


-- Ex3: process the list to give a frequency table. Pass the table and its max value to histogram'
histogram :: [Integer] -> String
histogram ls = histogram' max xs where
    xs = map length [filter (\x -> x == n) ls| n <- [0..9]]
    max = maximum xs

--Handles the printing, using maps and the value of max given to it.
histogram' :: Int -> [Int] -> String
histogram' 0 _ = "==========\n0123456789\n"
histogram' n ls = map (f n) ls ++ "\n" ++ histogram' (n-1) (map (g n) ls)

f :: Int -> Int -> Char
f n n'
    | n == n' = '*'
    | otherwise = ' '

g :: Int -> Int -> Int
g n n'
    | n == n' = n - 1
    | otherwise = n'
