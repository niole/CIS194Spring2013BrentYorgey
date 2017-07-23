module HW where

import Data.List

--Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 = foldl (\a -> \n -> if even n then (n - 2) * a else a) 1

--fun2 6 == 46
fun2 :: Integer -> Integer
fun2 n = sum $ filter (\n -> even n) $ takeWhile (\n -> n /= 1) $ iterate helper n
    where helper n
            | even n = n `div` 2
            | otherwise = 3 * n + 1

--Fold Tree

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree [] = Leaf
foldTree as = foldl insertNode Leaf $ formatMedianFirst as

formatMedianFirst :: Ord a => [a] -> [a]
formatMedianFirst as = takeOutMedian $ sortBy compare as
                        where takeOutMedian as = case drop (div (length as) 2) as of
                                                  [] -> []
                                                  (h : hs) -> h : takeOutMedian (take (div (length as) 2) as) ++ hs

getMedian :: [a] -> a
getMedian as = case drop (div (length as) 2) as of
                (h : _) -> h

insertNode :: Ord a => Tree a -> a -> Tree a
insertNode Leaf a = Node 0 Leaf a Leaf
insertNode (Node d x n y) a
  | a < n = updateDepth $ Node d (insertNode x a) n y
  | a > n = updateDepth $ Node d x n (insertNode y a)
  | otherwise = (Node d x n y) -- if not unique do nothing
        where updateDepth t = case t of
                              Leaf -> t
                              (Node _ Leaf n Leaf) -> Node 0 Leaf n Leaf
                              (Node _ Leaf n (Node e f g h)) -> Node (e + 1) Leaf n (Node e f g h)
                              (Node _ (Node e f g h) n Leaf) -> Node (e + 1) (Node e f g h) n Leaf
                              (Node _ (Node e f g h) n (Node i j k l)) -> Node ((max e i) + 1) (Node e f g h) n (Node i j k l)

-- More Folds
-- odd number of true -> true
xor :: [Bool] -> Bool
xor bs = not $ even $ length $ foldl keeper [] bs
        where keeper acc b = case b of
                        True -> b : acc
                        False -> acc

-- implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f as = reverse $ foldl mapper [] as
              where mapper acc a = f a : acc
