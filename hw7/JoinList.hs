{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Editor
import Sized
import Scrabble
import Buffer

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
dropJ n (Single a b)
        | n > 0 = Empty
        | otherwise = Single a b
dropJ n (Append s left right) = let sizeL = getSize $ size $ tag left
                                    sizeS = getSize $ size s
                                in case () of
                                  _ | n == sizeS -> Empty
                                    | n <= sizeL ->  (dropJ n left) +++ right --recurse into left
                                    | otherwise -> dropJ (n - sizeL) right --modify n and go right
--3


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n (Single a b)
        | n > 0 = Single a b
        | otherwise = Empty
takeJ n (Append s left right) = let sizeL = getSize $ size $ tag left
                                    sizeS = getSize $ size s
                                in case () of
                                  _ | n == sizeS -> Append s left right
                                    | sizeL >= n -> takeJ n left
                                    | otherwise -> left +++ (takeJ (n - sizeL) right)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

--E4

instance Buffer (JoinList (Score, Size) String) where
  toString a = case a of
                Empty -> ""
                Single _ b -> b
                Append s left right -> (toString left) ++ toString right

  fromString s = Single (scoreString s, Size 1) s

  line index t = indexJ index t

  replaceLine index ln t
        | index == 0 = (fromString ln) +++ (dropJ (index + 1) t)
        | otherwise =
          (takeJ (index - 1) t) +++ (fromString ln) +++ (dropJ (index + 1) t)

  numLines t = length $ jlToList t

  value t = case t of
            Empty -> 0
            Single (Score s, _) _ -> s
            Append (Score s, _) _ _ -> s

main = runEditor editor $ (Single (scoreString "cat", Size 1) "cat") +++ (Single (scoreString "bl", Size 1) "bl") +++ (Single (scoreString "blah", Size 1) "blah") +++ (Single (scoreString "you", Size 1) "you") +++ (Single (scoreString "hey", Size 1) "hey")
