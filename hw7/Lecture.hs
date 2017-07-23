{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Example
newtype Sum a = Sum a
        deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
  mempty  = Sum 0
  mappend = (+)

--the Challenges

--Bool instance of Monoid

newtype And a = And a
        deriving (Show)

--Since Bool is a data constructor and not a type class
--Can't derive the mappend operator && the way it was possible with + in Sum
--Therefore must implement mappend
instance Monoid (And Bool) where
  mempty = And True -- If concatenated elements already True, still True, if False, still False
  mappend x y = And $ getAnd x && getAnd y

getAnd :: And a -> a
getAnd (And a) = a

myAnd :: Bool
myAnd = getAnd . mconcat . map And $ [False]

newtype Or a = Or a
        deriving (Show)

instance Monoid (Or Bool) where
  mempty = Or False -- If concatenated elements are already True, will remain True, if False will remain False
  mappend x y = Or $ getOr x || getOr y

getOr :: Or a -> a
getOr (Or a) = a

myOr :: Bool
myOr = getOr . mconcat . map Or $ [False, False, True, False, False]

--Function instance of Monoid
--composing functions
--what is the composing function operator?
--what is the function type

newtype Func a = Func a
        deriving (Show)

getFunc :: Func (a -> a) -> (a -> a)
getFunc (Func a) = a

instance Monoid (Func (a -> a)) where
  mempty = Func (\n -> n)
  mappend x y = Func $ cpose (getFunc x) (getFunc y)

cpose :: (a -> b) -> (b -> c) -> (a -> c)
cpose a b = (\n -> b $ a n)

--run with number argument, e.g. runFunc 5
runFunc :: Num a => (a -> a)
runFunc = getFunc . mconcat . map Func $ [(1+), (2+)]
