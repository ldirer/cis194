module Scrabble where


import Data.Monoid
import Data.Char (toUpper)

data Score = Score Int deriving (Eq, Show)


instance Monoid Score where
    mempty = Score 0
    mappend (Score n) (Score p) = Score (n + p)




score :: Char -> Score
score c = case toUpper c of
            'A' -> Score 1
            'E' -> Score 1
            'I' -> Score 1
            'L' -> Score 1
            'N' -> Score 1
            'O' -> Score 1
            'R' -> Score 1
            'S' -> Score 1
            'T' -> Score 1
            'U' -> Score 1
            'D' -> Score 2
            'G' -> Score 2
            'B' -> Score 3
            'C' -> Score 3
            'M' -> Score 3
            'P' -> Score 3
            'F' -> Score 4
            'H' -> Score 4
            'V' -> Score 4
            'W' -> Score 4
            'Y' -> Score 4
            'K' -> Score 5
            'J' -> Score 8
            'X' -> Score 8
            'Q' -> Score 10
            'Z' -> Score 10
            _   -> Score 0
            -- _   -> error "This is not an official Scrabble character!"



scoreString :: String -> Score
scoreString s = foldl1 mappend $ map score s



