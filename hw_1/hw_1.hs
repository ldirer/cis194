import Data.List as List
import Test.QuickCheck

-- convert positive integers into a list of digits
toDigits :: Integer -> [Integer]
toDigits n | n < 10 = [n]
           | otherwise = mod n 10 : toDigits (n `div` 10)

-- same as toDigits but the list should be reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = List.reverse $ toDigits n

propToDigitsAndBack :: Integer -> Bool
propToDigitsAndBack n = concatMap show (toDigitsRev n) == show n




-- double every other number, starting from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = List.reverse . doubleEveryOtherl . List.reverse 

-- double every other number, starting from the left
doubleEveryOtherl :: [Integer] -> [Integer]
doubleEveryOtherl [] = []
doubleEveryOtherl [x] = [x]
doubleEveryOtherl (x1:x2:xs) = x1 : x2 * 2 : doubleEveryOtherl xs


-- Sum all digits in integers in list
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ map (sum . toDigits) xs

-- validate a credit card number
validate :: Integer -> Bool
validate n = mod (sumDigits $ doubleEveryOther $ toDigitsRev n) 10 == 0



type Peg = String
type Move = (Peg, Peg)
-- hanoi: given number of disks and names of pegs, return moves to 'win'.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = [] 
hanoi n p1 p2 p3 = hanoi (n - 1) p1 p3 p2 ++ [(p1, p3)] ++ hanoi (n - 1) p2 p1 p3


main :: IO()
main = do
    quickCheck propToDigitsAndBack
    print $ doubleEveryOther [1, 2, 3] == [1, 4, 3]
    print $ doubleEveryOther [8,7,6,5] == [16,7,12,5]
    print $ sumDigits [16,7,12,5] == 22
    print $ validate 4012888888881881 -- True
    print $ validate 4012888888881882 -- False
    -- print $ hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
    print $ hanoi 2 "a" "b" "c" == [("a", "b"), ("a", "c"), ("b", "c")]
    print $ length $ hanoi 15 "a" "b" "c"


-- ./hw_1
