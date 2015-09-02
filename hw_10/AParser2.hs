{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------


-- Better to actually read the lectures before trying the hw...


first :: (a -> b) -> (a, c) -> (b, c)
first f t = (f $ fst t, snd t)



instance Functor Parser where
    fmap f (Parser p) = Parser $ fmap (first f) . p




instance Applicative Parser where
    pure a = Parser (\s -> Just (a, s))
    (Parser fp) <*> (Parser p) = Parser run
        where run s = let res1 = fp s
                       in case res1 of 
                            Nothing        -> Nothing
                            Just (f, rem1) -> fmap (first f) (p rem1)



-- *AParser> runParser abParser "abcdef"
-- Just ((’a’,’b’),"cdef")
-- *AParser> runParser abParser "aebcdf"

abParser :: Parser (Char, Char)
abParser = fmap (\s s' -> (s, s')) (char 'a') <*> char 'b'


abParser_ :: Parser ()
abParser_ = fmap (\_ _ -> ()) (char 'a') <*> char 'b'



intPair :: Parser [Integer]
intPair = fmap (\a _ b -> [a, b]) posInt <*> char ' ' <*> posInt

instance Alternative Parser where
    empty = Parser (const Nothing)
    (Parser p1) <|> (Parser p2) = Parser (\s -> p2 s <|> p1 s)


intOrUppercase :: Parser ()
intOrUppercase = fmap (const ()) posInt <|> fmap (const ()) (satisfy isUpper)



