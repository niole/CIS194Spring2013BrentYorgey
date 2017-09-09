{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char


------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
--oneOrMore p = (:) <$> p <*> zeroOrMore p
oneOrMore p = liftA2 (:) p (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = liftA2 (++) parseFirst parseRest
        where parseFirst = oneOrMore $ satisfy isAlpha
              parseRest = zeroOrMore $ satisfy isAlphaNum

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

parseOpenParen :: Parser Char
parseOpenParen = char '('

parseClosedParen :: Parser Char
parseClosedParen = char ')'

parseAtom :: Parser Atom
parseAtom = toIdent <$> ident <|> toNum <$> posInt
        where toIdent s = I s
              toNum n = N n

--parseSExpList :: Parser [SExpr]
--parseSExpList = joiner <$> parseOpenParen <*> spaces <*> parseAtom <*> spaces <*> parseClosedParen
--                 where joiner a b c d e = a : b ++ (c : d ++ [e])
--
--parseSExpr :: Parser SExpr
--parseSExpr = parseAtom <|> parseSExpList