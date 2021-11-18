--Ex1
toDigits :: Integer -> [Integer]
toDigits n = [read [x] | x <- show n] -- read expects a string, and x is a char, so i make it a list of one char
-- show n takes 1234 and returns "1234"


toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

--Ex2 Doubles every other digit, read from right to left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = reverse (helper (reverse lst))
    where
        helper [] = []
        helper (x:[]) = [x]
        helper (x:(y:xs)) = x : 2 * y : helper(xs)


--Ex3 Takes sum of digits of integers in a list
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits(x)) + sumDigits(xs)
    where
        sum [] = 0
        sum (x:xs) = x + (sum xs)


--Ex4 Validates a credit card checksum
validate :: Integer -> Bool
validate n
    | sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0 = True
    | otherwise = False


--Ex5 Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)
-- not as simple as taking n/2 since you need to preserve the ordering of the discs



