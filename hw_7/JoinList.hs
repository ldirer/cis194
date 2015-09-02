{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ Empty = a
Empty +++ b = b
a +++ b = Append (tag a <> tag b) a b


-- Get the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = error "Wat. We should return the neutral element for the monoid here."
tag (Single m _) =  m
tag (Append m _ _) =  m

-- Exercise 2

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag


indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ a) | n > 0 = Nothing -- We shouldn't be reaching that a lot (only when calling indexJ directly on a Single jl)
                      | n == 0 = Just a
indexJ n (Append m jll jlr) | n > (getSize $ size m) = Nothing
                            | n > ml  = indexJ (n - ml) jlr
                            | n <= ml = indexJ n jll
                                where  ml = jlSize jll
                                       -- mr = getSize $ size $ tag jlr

-- Functions provided to help test indexJ, see pdf.
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


testIndexJ :: (Sized b, Monoid b, Eq a) => Int -> JoinList b a -> Bool
testIndexJ i jl = (indexJ i jl) == (jlToList jl !!? i)
    
-- TODO: test this fragile code...


-- listToJoinList


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n <= 0 = jl 
dropJ _ (Single _ _) = Empty
dropJ n (Append _ jll jlr)  | n <= ml = (dropJ n jll) +++ jlr 
                            | n > ml = dropJ ml jlr
                                where ml = jlSize jll


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ s@(Single _ _) = s
takeJ n (Append _ jll jlr) | n <= ml = takeJ n jll
                           | otherwise = jll +++ takeJ (n - ml) jlr
                               where ml = jlSize jll




scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s





-- Exercise 4
 
instance Buffer (JoinList (Score, Size) String) where
    toString jl = concat $ jlToList jl 
    fromString s = Single (scoreString s, Size (length s)) s
    line n buf = 






