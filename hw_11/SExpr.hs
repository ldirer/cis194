{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char
import Control.Monad
-- import Text.Parse (parseInt)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
-- zeroOrMore p =  (fmap (\a b -> a:b) p <*> (zeroOrMore p)) <|> (pure [])
-- Even better:
zeroOrMore p =  (oneOrMore p) <|> (pure [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = fmap (\a b -> a:b) p <*> (zeroOrMore p)

-- Some test cases:


-- *AParser> runParser (zeroOrMore (<satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
-- *AParser> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")



------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = fmap (\a b -> a ++ b) (oneOrMore (satisfy isAlpha)) <*> zeroOrMore (satisfy isAlphaNum)
-- Better, from the solution:
-- ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- Some test cases.
-- *AParser> runParser ident "foobar baz"
-- Just ("foobar"," baz")
-- *AParser> runParser ident "foo33fA"
-- Just ("foo33fA","")
-- *AParser> runParser ident "2bad"
-- Nothing
-- *AParser> runParser ident ""
-- Nothing


------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


-- No: here we just get a list of Atom objects (loosing the nesting information)
-- parseSExpr :: Parser SExpr
-- -- On parse un atome. Ensuite oneOrMore: si c'est ok on utilise Comb, sinon A.
-- parseSExpr = (fmap Comb ((fmap (\a b -> a : b) parseAtom) <*> (oneOrMore parseAtom))) <|> parseAtom
--     where parseAtom :: Parser SExpr
--           parseAtom = fmap A (spaces *> (zeroOrMore (char '(')) *> spaces *> (fmap N posInt <|> fmap I ident)) <* spaces <* (zeroOrMore (char ')') <* spaces)
--           


parens :: Parser a -> Parser a
parens p = char '(' *> p <* char ')'

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = spaces *>
             (     A    <$> parseAtom
               <|> Comb <$> parens (oneOrMore parseSExpr)
             )
             <* spaces
             


main :: IO ()
main = do
    print $ runParser parseSExpr "( lots of (   spaces  in   )   this ( one ) )"
    print $ runParser parseSExpr "(this ( one ) )"
    print $ runParser parseSExpr "lots of (   spaces  in   )   this ( one ) )"






