{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}



-- import ExprT
import Test.QuickCheck
import Parser (parseExp)
import StackVM

class Expr a where
    mul :: a -> a -> a
    add :: a -> a -> a
    lit :: Integer -> a

-- Previous exercise, conflict with exercise 5 so commented out.
-- instance Expr ExprT where
--     mul = Mul
--     add = Add
--     lit = Lit
-- 
-- eval :: ExprT -> Integer
-- eval (Lit n) = n
-- eval (Add n m) = eval n + eval m
-- eval (Mul n m) = eval n * eval m
-- 
-- 
-- evalStr :: String -> Maybe Integer
-- evalStr s = case parser s of
--               Nothing -> Nothing
--               Just e -> Just (eval e)
--               where parser = parseExp Lit Add Mul


instance Expr Integer where
    mul = (*)
    add = (+)
    lit = id

instance Expr Bool where
    mul = (&&)
    add = (||)
    lit = (>0)


newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
     mul = min
     add = max 
     lit = MinMax 


instance Expr Mod7 where
     mul = mulMod7
         where mulMod7 (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
     add = addMod7
         where addMod7 (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
     lit = Mod7 . (`mod` 7)


instance Expr Program where
    mul x y = x ++ y ++ [Mul]
    add x y = x ++ y ++ [Add]
    lit x = [PushI x]


compile :: String -> Maybe Program
compile = (parseExp lit add mul) :: String -> Maybe Program



testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7





main :: IO()
main = do 
    -- print $ eval (Mul (Add (Lit 5) (Lit 3)) (Add (Lit 2) (Lit 2))) == 32
    -- print $ evalStr "(Mul (Add (Lit 5) (Lit 3)) (Add (Lit 2) (Lit 2)))" == Just 32
    -- print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
    -- print $ evalStr "(Mul (Add (Lit 2) (Lit 3)) (Lit 4))" == Just 20
    -- print $ ((mul (add (lit 2) (lit 3)) (lit 4)) :: ExprT) == (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    print $ testInteger
    print $ testBool
    print $ testMM
    print $ testSat





