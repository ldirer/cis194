import Data.Tuple.Curry (uncurryN)
import Test.QuickCheck
import Data.List (maximum, group, sort)

-- skipMultiples :: [a] -> Int -> [a]
-- skipMultiples xs n = filter (/=0) $ scanl1 (+) xs --((`mod` n) . (+)) xs


everyNth :: [a] -> Int -> [a]
everyNth xs n = case drop (n - 1) xs of
                  y:ys -> y : everyNth ys n
                  [] -> []

skips :: [a] -> [[a]]
-- skips xs = map (\n -> everyNth xs n) [1..length(xs)]
skips xs = map (everyNth xs) [1..length(xs)]


propSkipsPreservesLength :: [a] -> Bool
propSkipsPreservesLength xs = (length . skips) xs == length xs
-- Nope:
-- propSkipsPreservesLength = (==) (length . skips) length


-- localMaxima :: [Integer] -> [Integer]
-- localMaxima [] = []
-- localMaxima [_] = []
-- localMaxima [_, _] = []
-- localMaxima (x1:t@(x2:x3:_)) | x2 > x1 && x2 > x3 = x2 : localMaxima t
--                              | otherwise          = localMaxima t


lastIsMax :: Integer -> Integer -> Integer -> Bool
lastIsMax x y z = x < z && y < z

-- lastIsMax :: (Integer, Integer) -> Integer -> Bool
-- lastIsMax (x, y) z = x < z && y < z


filterMask :: [a] -> [Bool] -> [a]
filterMask xs bs = map fst $ filter snd (zip xs bs)

localMaxima :: [Integer] -> [Integer]
--localMaxima l@(x:xs) = filterMask (map (map lastIsMax (zip (take (n - 1) xs) (take (n-2) l))) l) l
    --where n = length l
localMaxima [] = []
localMaxima l@(_:xs) = filterMask xs (map (uncurryN lastIsMax) (zip3 (tail xs) (take (n - 2) l) (take (n - 1) xs)))
    where n = length l

-- histogram :: [Integer] -> [Integer]
-- histogram xs = foldl counter acc xs
--     where counter :: [Integer] -> Integer -> [Integer]
--           counter (a:as) x | x == 0 = (a + 1) : as
--                            | otherwise = a : counter as (x - 1)
--           counter _ _ = []
--           acc = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

histogram :: [Integer] -> [Integer]
histogram = (map (pred . toInteger . length)) . group . sort . ((++) [0..9])
          
-- [10..1] -> []. We need to somehow specify a step if we want reverse [1..10]. See enumFromThenTo.
-- Build histogram lines from the list of counts.
buildLines :: [Integer] -> String
-- buildLines xs = foldl (++) "" $ map (buildLine xs) [max_x, max_x - 1..1] 
-- Better first line:
buildLines xs = unlines $ map (buildLine xs) [max_x, max_x - 1..1] 
    where max_x = maximum xs
          buildLine :: [Integer] -> Integer -> String
          buildLine xs i = map starify (map (>= i) xs) -- ++ "\n": not required with unlines
              where starify :: Bool -> Char
                    starify True = '*'
                    starify False = ' '
                            
pltHistogram :: [Integer] -> String
pltHistogram xs = (buildLines . histogram) xs ++ "==========\n0123456789\n"

-- Build the histogram from the bottom up.
-- Put a star for every indice that appears in the list. Remove that element.
-- Again with the next line until there's nothing to display.
-- So it's: build this line, take what you need. Stack an histogram with what's left if any.
-- histogram xs = case xs of
--                 [] -> ""
                 


main :: IO()
main = do 
    print $ everyNth [1, 2, 3, 4, 5, 6] 2
    print $ skips [1, 2, 3, 4, 5, 6]
    print $ skips "ABCD"
    print $ skips "hello!"
    print $ skips [1]
    print $ skips [[True, False]]
    print $ skips ([]::[Int])
    quickCheck (propSkipsPreservesLength :: [Integer] -> Bool)
    print $ localMaxima [2, 9, 5, 6, 1]
    print $ localMaxima [2, 3, 4, 1, 5]
    print $ localMaxima [1, 2, 3, 4, 5]
    print $ histogram [1, 2, 2, 3, 3, 3, 4, 4, 4, 4] -- [0, 1, 2, 3, 4, 0, 0, 0, 0, 0]
    putStr $ pltHistogram [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
    putStr $ pltHistogram [1,4,5,4,6,6,3,4,2,4,9]




