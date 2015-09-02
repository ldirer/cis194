{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Parser (parseExp)
import qualified Data.Map.Strict as M


class Expr a where
    mul :: a -> a -> a
    add :: a -> a -> a
    lit :: Integer -> a


class HasVars a where
    var :: String -> a


data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance Expr VarExprT where
    mul = Mul
    add = Add
    lit = Lit

instance HasVars VarExprT where
    var = Var




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

type VarMap = (M.Map String Integer -> Maybe Integer)

instance HasVars VarMap where
    -- Better:
    var = M.lookup
    -- var s = f where f mapping = M.lookup s mapping

-- TODO: obviously this boilerplate code is not the way to go.
instance Expr VarMap where
    -- Look at examples to understand what add and mul are meant to do.
    lit n _ = Just n
    add varMap1 varMap2 = \map -> case (varMap1 map, varMap2 map) of
                                    (Nothing, _) -> Nothing
                                    (_, Nothing) -> Nothing
                                    (Just x, Just y) -> Just (x + y)

    mul varMap1 varMap2 = \map -> case (varMap1 map, varMap2 map) of
                                    (Nothing, _) -> Nothing
                                    (_, Nothing) -> Nothing
                                    (Just x, Just y) -> Just (x * y)



withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs




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
    print $ withVars [("x", 6)] $ add (lit 3) (var "x")
    print $ withVars [("x", 6)] $ add (lit 3) (var "y")
    print $ withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))











