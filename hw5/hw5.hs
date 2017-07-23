module HW where

import ExprT as E
import Parser
import StackVM as S

-- E1: write v1 of the calculator
eval :: ExprT -> Integer
eval (Lit n) = n
eval (E.Add a b) = (eval a) + (eval b)
eval (E.Mul a b) = (eval a) * (eval b)

-- E2
evalStr :: String -> Maybe Integer
evalStr e = case parseExp Lit E.Add E.Mul e of
            Just foundExp -> Just $ eval foundExp
            _ -> Nothing

-- E3

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr Integer where
  lit a = a
  mul a b = a * b
  add a b = a + b

-- E4

instance Expr Bool where
  lit a = a > 0
  mul a b = a && b
  add a b = a || b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit a = MinMax a
  mul (MinMax a) (MinMax b) = MinMax $ min a b
  add (MinMax a) (MinMax b) = MinMax $ max a b

instance Expr Mod7 where
  lit a = Mod7 $ mod a 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7

testExp :: Expr a => Maybe a
testExp = Just $ lit 1

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- E5

class Push a where
  push :: a -> StackExp

instance Push Bool where
  push b = PushB b

instance Push Integer where
  push n = PushI n

compile :: String -> Maybe Program
compile e = case parseExp E.Lit E.Add E.Mul e of
        Just found -> Just $ getComponents found
        _ -> Nothing
        where getComponents (Lit n) = [push n]
              getComponents (E.Mul a b) = S.Mul : getComponents a ++ getComponents b
              getComponents (E.Add a b) = S.Add : getComponents a ++ getComponents b
