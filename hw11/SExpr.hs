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

appendChar :: String -> Char -> String
appendChar s c = s ++ [c]

parseWParens :: Parser SExpr
parseWParens = parseStart $ oneOrMoreParseSExprs parseMiddle <* parseEnd
               where parseStart p = liftA2 appendChar spaces parseOpenParen *> p
                     parseMiddle =
                       combineSExprParsers
                                (combineSExprParsers (optionalParseSExprs parseAtoms) parseWParens)
                                (optionalParseSExprs parseAtoms) <|> parseAtoms
                     parseEnd = liftA2 appendChar spaces parseClosedParen

oneOrMoreParseSExprs :: Parser SExpr -> Parser SExpr
oneOrMoreParseSExprs p = (\es -> Comb es) <$> oneOrMore p

optionalParseSExprs :: Parser SExpr -> Parser SExpr
optionalParseSExprs p = (\es -> Comb es) <$> zeroOrMore p

parseSExpr :: Parser SExpr
parseSExpr = parseAtoms <|> parseWParens

combineSExprParsers :: Parser SExpr -> Parser SExpr -> Parser SExpr
combineSExprParsers a b = liftA2 combineSExpr a b

combineSExpr :: SExpr -> SExpr -> SExpr
combineSExpr (Comb as) (Comb bs) = Comb (as ++ bs)
combineSExpr (Comb as) (A a) = Comb (as ++ [A a])
combineSExpr (A a) (Comb as) = Comb ((A a):as)

asComb :: [Atom] -> SExpr
asComb as = Comb $ toA <$> as

toA :: Atom -> SExpr
toA a = A a

parseClosedParen :: Parser Char
parseClosedParen = char ')'

parseAtom :: Parser Atom
parseAtom = toIdent <$> ident <|> toNum <$> posInt
        where toIdent s = I s
              toNum n = N n

parseAtoms :: Parser SExpr
parseAtoms = asComb <$> (oneOrMore $ spaces *> parseAtom)
