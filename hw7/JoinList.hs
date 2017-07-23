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
indexJ index (Append s left right) = indexJHelper (getSize $ size $ tag left) (getSize $ size s)
  where indexJHelper sizeM sizeS
          | index >= sizeS = Nothing
          | index < sizeM = indexJ index left
          | index >= sizeM = indexJ (index - sizeM) right

x = Append (Size 4) (Append (Size 2) (Single (Size 1) "a") (Single (Size 1) "b")) (Append (Size 2) (Single (Size 1) "c") (Single (Size 1) "d"))

--makeJSTree :: (Sized m, Monoid m) => [a] -> JoinList m a
--makeJSTree es = foldl (+++) Empty $ map (\e -> Single (Size 1) e) es
