{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where


import Data.Monoid
import Data.Tree
import Data.List (intercalate, sort)
import Employee

-- treeFold :: (a -> b -> b) -> b -> Tree a -> b
-- treeFold f b (Node a []) = f a b
-- treeFold f b (Node a trees) = foldl (treeFold f) (f a b) trees


glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ efun) (GL es f) = GL (e:es) (f + efun)


instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)





moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ f1) b@(GL _ f2) | f1 > f2   = a
                                | otherwise = b


treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a []) = f a []
treeFold f (Node a trees) = f a (map (treeFold f) trees)


-- The recursion step is tricky: we need to compute a lot of information.
-- So we have 2 optimal guest lists: one with the boss invited, the other without.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- If we take the boss we can't take previous level bosses
-- If we don't take the boss we take the combination of all "best of w/ boss vs w/o boss".
-- Tuple: (glboss, glnoboss)
nextLevel e glpairs = (glCons e (mconcat (map snd glpairs)), mconcat (map (uncurry moreFun) glpairs))

sumFun :: Employee -> [Integer] -> Integer
sumFun (Emp _ efun) xs = efun + sum xs


-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun company = uncurry moreFun $ treeFold nextLevel company

showFormatted :: GuestList -> String
-- Coulda shoulda used unlines instead of intercalate "\n".
showFormatted (GL es fun) = intercalate "\n" (("Total fun: " ++ show fun) : sort (map empName es))

main :: IO()
main = do 
    let gl = maxFun testCompany
     in do putStrLn $ showFormatted gl  -- Expected: 26
           wholeCompanyStr <- readFile "company.txt"
           let wholeCompany = read wholeCompanyStr :: (Tree Employee)
            in putStrLn $ showFormatted (maxFun wholeCompany)
    print $ treeFold sumFun testCompany  -- Expected: 46








