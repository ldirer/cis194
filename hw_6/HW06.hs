{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

-- Turn off warnings about missing implementations of typeclass methods.
{-# OPTIONS_GHC -fno-warn-missing-methods #-}



module HW06 where

import Data.List
import Data.Functor
import Test.QuickCheck

-- Exercise 1 -----------------------------------------
-- NAIVE implementation.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------


fibs2 :: [Integer]
-- fibs2 = map (\(n, m) -> n + m) $ zip (0 : 0 : (tail fibs2)) (0 : 1 : (init fibs2))
-- Better
fibs2 = zipWith (+) (0 : 0 : (tail fibs2)) (0 : 1 : (init fibs2))

-- Exercise 3 -----------------------------------------


-- En fait on va définir les séries entières (Stream)!

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 20 $ streamToList s)
             ++ ",..."


instance Eq (Stream Integer) where
    (==) sa sb = let as = streamToList sa
                     bs = streamToList sb
                  in (take 20 as) == (take 20 bs) -- Slightly lame, enables to run some tests.


streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

listToStream :: [Integer] -> Stream Integer
listToStream [] = streamRepeat 0
listToStream (a:as) = Cons a $ listToStream as



-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))




-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0



ruler :: Stream Integer
ruler = streamMap (testDiv . (+1)) nats

testDiv :: Integer -> Integer
testDiv n = last $ takeWhile (\k -> n `mod` (2^k) == 0) [0..]

-- Alternative way of getting `ruler`


interleaveStreams :: Stream a -> Stream a -> Stream a
-- interleaveStreams (Cons a as) (Cons b bs) = Cons a ((Cons b) (interleaveStreams as bs))
-- Same, but better:
interleaveStreams (Cons a as) bs = Cons a (interleaveStreams bs as)

-- "clever way to implement ruler that does not do any divisibility testing"?
-- I did not really know what to do. The solution (oh my):
ruler_ :: Stream Integer
ruler_ = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler_)





-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))
           



instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate s = streamMap (* (-1)) s
    (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
    -- Using P * Q = a_0 * Q  + b_0 * X * (a_1 + a_2X +...) + X^2 * (a_1 + a_2 X...) * (b_1 + b_2 X + ..)
    (*) (Cons a as) q@(Cons b bs) = streamMap (*a) q + (Cons 0 (streamMap (*b) as + (Cons 0 (as * bs))))
    
instance Fractional (Stream Integer) where
    -- See pdf with the 'formula'
    -- Ce n'est pas une division de polynômes mais de séries entières!
    (/) p@(Cons a as) q@(Cons b bs) | b == 0 = (as / bs)
                                    | otherwise = Cons (a `div` b) (streamMap (`div` b) (as - bs * (p / q)))


propMultiplyStreams ::  [Integer] -> [Integer] -> Bool
propMultiplyStreams as bs = let sa = listToStream as 
                                sb = listToStream bs
                             in length as > 20 || length bs > 20 || (sa * sb) == (sb * sa)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7: with matrices.


-- 2x2 matrices.
data Matrix = Matrix Integer Integer Integer Integer deriving (Eq)

instance Show Matrix where
    show (Matrix a b c d) =  foldl1' (++) $ intersperse " " ["", show a, show b, "\n", show c, show d, "\n"]

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix a2 b2 c2 d2) = let a' = a * a2 + b * c2
                                                    b' = a * b2 + d2 * b
                                                    c' = a2 * c + c2 * d
                                                    d' = b2 * c + d2 * d
                                                 in Matrix a' b' c' d'


fmatrix :: Matrix 
fmatrix = Matrix 1 1 1 0


fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = let (Matrix fn _ _ _) = (fmatrix ^ (n - 1))
          in fn



main :: IO()
main = let p :: Stream Integer
           p = Cons 1 (Cons 1 (Cons 1 (streamRepeat 0)))
           m = Matrix 1 2 3 4
           i = Matrix 1 0 0 1
        in do
            print $ p
            print $ p + p
            print $ p * p -- 1, 2, 3, 2, 1 expected.
            print $ p / p -- 1, 2, 3, 2, 1 expected.
            quickCheck propMultiplyStreams
            print $ m
            print $ m * m
            print $ i
            print $ i * i
            print $ take 40 $ map fib4 [0..]

            





