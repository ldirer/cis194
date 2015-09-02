import Test.QuickCheck
import Data.List

fun1 :: [Integer] -> Integer
fun1 xs = product $ map (+(-2)) $ filter even xs


fun2 :: Integer -> Integer
fun2 n = let evens = takeWhile (\k -> even k && k > 0) $ iterate (`div` 2) n
             n_odd = (last evens) `div` 2
             in case n_odd of
                  1 -> sum evens
                  _ -> sum evens + fun2 (3 * n_odd + 1)


fun2_ :: Integer -> Integer
fun2_ 1 = 0
fun2_ n | even n = n + fun2_ (n `div` 2)
        | otherwise = fun2_ (3 * n + 1)

-- Crashes ghc with Stackoverflow!! TODO: WTH?
propFun2StillWorks :: Integer -> Bool
propFun2StillWorks n = fun2_ n == fun2 n



-- Exercise 2: folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = foldr insertTree Leaf xs


insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node h l y r) = case (l, r) of
                            (Leaf, _) -> Node 1 (insertTree x l) y r
                            (_, Leaf) -> Node 1 l y (insertTree x r)
                            (Node hl _ _ _, Node hr _ _ _) -> 
                                case compare hl hr of
                                  LT -> Node h (insertTree x l) y r
                                  GT -> Node h l y (insertTree x r)
                                  EQ -> 
                                      let new_l@(Node hl_with_x _ _ _) = insertTree x l
                                          new_r@(Node hr_with_x _ _ _) = insertTree x r
                                         in case compare hl_with_x hr_with_x of
                                              LT -> Node (hl_with_x + 1) new_l y r
                                              GT -> Node (hr_with_x + 1) l y new_r
                                              EQ -> Node (hr_with_x + 1) l y new_r
                                              

--TODO: check for a better solution (or come up with one)!!
-- Exercise 3 of HW 4 has been covered in RWH


-- Exercise 4: Sieve of Sundaram

-- http://en.wikipedia.org/wiki/Sieve_of_Sundaram 
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let toDrop = (takeWhile (<= n) $ map (\(i, j) -> i + j + 2 * i * j) $ zip [1..] [1..])
                   in map (\k -> 2 * k + 1) ([1..n] \\ toDrop)
-- TODO: Check a solution using cartesian product (?)


main :: IO()
main = do
    print $ fun1 [3..6]
    print $ fun2 10
    print $ fun2_ 10
    print $ foldTree "ABCDEFGHIJ" 
    -- print $ foldTree "ABCDEFGHIJKLMNOPQRSTU" 
    print $ sieveSundaram 10
       




