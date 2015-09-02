{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Control.Applicative
import Data.List

choose :: Integral a => a -> a -> a
n `choose` k | k < 0     = 0
             | k > n     = 0
             | k == 0    = 1
             | otherwise = choose (n-1) (k-1) * n `div` k 
            


------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

-- testDie :: DieValue -> Int


------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

drawDices :: Army -> Rand StdGen [DieValue]
drawDices a = reverse . sort <$> replicateM a die


sumBool :: [Bool] -> Int
sumBool = foldl (flip ((+) . fromEnum)) 0

-- fight returns the Battlefield of dead fellows.
fight :: [DieValue] -> [DieValue] -> Rand StdGen Battlefield
-- fromEnum converts a bool to an Int.
fight att def = let nDeadDef = sumBool $ zipWith (>) att def
                    nFights = min (length att) (length def)
                 in return $ Battlefield (length att - (nFights - nDeadDef)) (length def - nDeadDef)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
    attDraw <- drawDices $ attackers bf
    defDraw <- drawDices $ defenders bf
    casualties <- fight attDraw defDraw
    return $ Battlefield (attackers bf - attackers casualties) (defenders bf - defenders casualties)

attackersWon :: Battlefield -> Bool
attackersWon bf = defenders bf == 0 && attackers bf > 0

defendersWon :: Battlefield -> Bool
defendersWon bf = attackers bf <= 2

battleIsOver :: Battlefield -> Bool
battleIsOver bf = attackersWon bf || defendersWon bf

-- battle until no defender remains or fewer than 2 attackers.
invade :: Battlefield -> Rand StdGen Battlefield
invade bf | battleIsOver bf = return bf
          | otherwise       = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    bfs <- replicateM 1000 (invade bf)
    return $ (fromIntegral (sumBool (map attackersWon bfs)) :: Double) / (fromIntegral (length bfs) :: Double)

-- Exercise 5

maxDiceRoll :: Int -> Rand StdGen DieValue
maxDiceRoll n = (replicateM n die) >>= return . maximum


maxRollsLaw :: Int -> IO ()
maxRollsLaw n = (evalRandIO (replicateM 100000 (maxDiceRoll n))) >>= print . (map (\(x, y) -> ((unDV . head) x, y))) . hist
    where hist s = map (\x->([head x], (fromIntegral (length x) :: Double) / (fromIntegral (length s)))) . group . sort $ s

-- This function seems to be correct.
exactMaxLaw :: Int -> Int -> Double
exactMaxLaw n i = (fromIntegral (i ^ n - (i - 1) ^ n) :: Double) / (fromIntegral (6 ^ n) :: Double)

-- P(X_jth_max(n) = i)
-- This function/formula is not correct.
exactJthLaw :: Int -> Int -> Int -> Double
exactJthLaw n j i = (fromIntegral (n `choose` (j - 1)) :: Double) * ((1 - (di - 1) / 6) ** (dj - 1)) * (exactMaxLaw (n - j) i)
    where di = fromIntegral i :: Double
          dj = fromIntegral j :: Double


exactSuccessProb_ :: Battlefield -> Double
exactSuccessProb_ (Battlefield att def) = undefined
    
-- ((fromIntegral att) :: Double) / (fromIntegral def :: Double)



-- Not-a-closed-form solution (much better!)
exactSuccessProb :: Battlefield -> Double
exactSuccessProb f
    | d < 1 = 1.0
    | a < 2 = 0.0
    | otherwise = ((exactSuccessProb $ Battlefield a (d-2)) + (exactSuccessProb $ Battlefield (a-1) (d-1)) + (exactSuccessProb $ Battlefield (a-2) d)) / 3
    where
        d = defenders f
        a = attackers f


main :: IO ()
main = do
    prob <- evalRandIO $ successProb $ Battlefield 5 2
    print prob
    print $ exactSuccessProb $ Battlefield 5 2
    prob <- evalRandIO $ successProb $ Battlefield 5 0
    print prob
    print $ exactSuccessProb $ Battlefield 5 0
    prob <- evalRandIO $ successProb $ Battlefield 5 5
    print prob
    print $ exactSuccessProb $ Battlefield 5 5
    maxRollsLaw 2
    print $ map (exactJthLaw 2 1) [1..6]
    print $ sum $ map (exactJthLaw 2 1) [1..6]
    print $ map (exactMaxLaw 2) [1..6]
    print $ sum $ map (exactMaxLaw 2) [1..6]
    print $ exactSuccessProb $ Battlefield 5 2
    




