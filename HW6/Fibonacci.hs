{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
-- Why does this not type check? I'm not sure why + fails to detect its working between
-- two integers, and not two streams.
-- Ex1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map (\n -> fib n) [0..]


-- Ex2
fibs2 :: [Integer]
fibs2 = [0,1] ++ fibs2' 0 1 where
    fibs2' a b = [a + b] ++ fibs2' b (a + b)


-- Ex3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x s) = [x] ++ streamToList s


-- this says something stricter then Stream a is a instance of Show.
-- It says for Stream a to be an instance of Show, its elements
-- must also be an instance of Show.
instance Show a => Show (Stream a) where
    show s = show $ take 20 $ streamToList s


-- Ex4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f1 s@(Stream h t) = Stream (f1 h) (streamMap f1 t)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream s (streamFromSeed f (f s))


-- Ex5
nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0


interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams s1@(Stream h1 t1) s2 = Stream h1 $ interleaveStreams s2 t1
-- to fix my issue, i edit interleaveStreams to yield a head without forcing both streams
-- this implementation below, causes the problem outlined further below.
-- interleaveStreams s1@(Stream h1 t1) s2@(Stream h2 t2) = Stream h1 $ Stream h2 $ interleaveStreams t1 t2 

ruler :: Stream Integer
ruler = ruler' 0


ruler' :: Integer -> Stream Integer
ruler' n = interleaveStreams (streamRepeat n) (ruler' (n + 1))


{-
ruler
ruler' 0
interleaveStreams (streamRepeat 0) (ruler' (n + 1))
interleaveStreams (Stream 0 (streamRepeat 0)) (interleaveStreams (streamRepeat (n + 1) (ruler' (n + 2) )))
interleaveStreams (Stream 0 (streamRepeat 0)) .. here's the issue. To evaluate interleaveStreams,
it forces both the head and the tail and the infinite loop happens.
-}


-- Ex6
x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Stream n (streamRepeat 0)

    negate = streamMap (\x -> negate x)

    (+) s1@(Stream h1 t1) s2@(Stream h2 t2) = Stream (h1 + h2) (t1 + t2)

    (*) s1@(Stream h1 t1) s2@(Stream h2 t2) = Stream (h1 * h2) (ns1 + ns2) where
        ns1 = streamMap (\x -> x * h1) t2
        ns2 = t1 * s2


divide :: (Stream Integer) -> (Stream Integer) -> (Stream Integer)
divide s1@(Stream h1 t1) s2@(Stream h2 t2) = 
    Stream (div h1 h2) $ streamMap (\x -> div x h2) (t1 - (divide s1 s2) * t2)


fibs3 :: Stream Integer
fibs3 = divide x (1 - x - x^2)


-- Ex7
data Matrix = Matrix Integer Integer Integer Integer -- tl tr bl br
    deriving Show 

instance Num Matrix where
    (*) (Matrix tl1 tr1 bl1 br1) (Matrix tl2 tr2 bl2 br2) = 
            Matrix (tl1 * tl2 + tr1 * bl2) (tl1 * tr2 + tr1 * br2) (bl1 * tl2 + br1 * bl2) (bl1 * tr2 + br1 * br2)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = (\(Matrix tl tr bl br) -> bl) m where
    m = (Matrix 1 1 1 0)^n 




