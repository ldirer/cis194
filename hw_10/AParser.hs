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


first :: (a -> b) -> (a, c) -> (b, c)
first f x = ((f . fst) x, snd x)


-- TODO: check solution for a simpler/neater implementation.
instance Functor Parser where
    fmap f p = Parser g
        where g x = let runp = runParser p in
                      case runp x of
                        Nothing -> Nothing
                        Just t  -> Just (first f t)


                    
-- Exercise 2
-- TODO: voir comment on gère les Maybe en vrai...
-- TODO: revoir pourquoi dans <*> un truc produit des fonctions...
instance Applicative Parser where
    pure a = Parser f
        where f xs = Just (a, xs) 
    p1 <*> p2 = Parser f
        where f xs = let r1 = runParser p1 xs in
                         case r1 of
                           Nothing                 -> Nothing
                           -- _       -> first (\x -> x (++) . p2) r1
                           Just (funParsed1, remain1) -> 
                               let r2 = runParser p2 remain1 in
                                   case r2 of
                                     Nothing                 -> Nothing
                                     Just (valParsed2, remain2) -> Just (funParsed1 valParsed2, remain2)
                                                          





