import Sized
--primitive word processing engine for Charles Dickens
--tracks total words while document in progress
--Buffer acts as the interface between BE and FE


--m tracks Monoidal annotations to the JoinList
--the annotation at the root of the JoinList equal
--the combination of Single nodes' annotations
--Emptys have annotations of 'mempty'
data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

--E1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) j k = Append (mappend (tag j) (tag k)) j k

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

--E2
--1

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single _ a) = Just a
indexJ index (Append s left right) = helper (getSize $ size $ tag left) (getSize $ size s)
  where helper sizeM sizeS
          | index >= sizeS = Nothing
          | index < sizeM = indexJ index left
          | index >= sizeM = indexJ (index - sizeM) right

x = (+++) ( (+++) (Single (Size 1) "a") (Single (Size 1) "b")) ((+++) (Single (Size 1) "c") (Single (Size 1) "d"))

--2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ _ (Single a b) = Single a b
dropJ n (Append s left right) = let sizeL = getSize $ size $ tag left
                                    sizeR = getSize $ size $ tag right
                                    sizeS = getSize $ size s
                                in case () of
                                  _ | (n - sizeS) == 0 -> Empty
                                    | (n - sizeL) == 0 -> right
                                    | (n - sizeL) < 0 -> Append s (dropJ n left) right --recurse into left
                                    | otherwise -> dropJ (n - sizeL) right --modify n and go right

