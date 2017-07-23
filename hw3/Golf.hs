module Golf where

import Data.List
import Data.Ord

--Hopscotch

--outputs a list of lists where the nth list contains only every nth
--element from the input list
skips :: [a] -> [[a]]
skips [] = []
skips as = getSkips 0
                where getSkips step
                        | step < (length as) = getInterimSkips step as : getSkips (step + 1)
                        | otherwise = []

getInterimSkips :: Int -> [a] -> [a]
getInterimSkips _ [] = []
getInterimSkips step as = case (drop step as) of
                          [] -> []
                          (b : bs) -> b : getInterimSkips step bs

--Local Maxima

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima (a : [b]) = []
localMaxima (a : b : c : cs)
        | b > a && b > c = b : localMaxima (c : cs)
        | otherwise = localMaxima (b : c : cs)

--Histogram

histogram :: [Integer] -> String
histogram ns = let hist = groupByInt ns
                   maxHeight = getMaxHeight hist
                   maxWidth = getMaxWidth hist
                   in formatNumbers maxWidth ++ "\n" ++ (formatHistogram $ map (\row -> createRow hist row 0) [0..maxHeight])

formatHistogram :: [String] -> String
formatHistogram hist = createMany "=" ((toInteger . length) h - 1) ++ "\n" ++ foldl (\a -> \r -> a ++ r ++ "\n") ("") hist
                        where (h : hs) = hist

-- creates elements in row
-- elements that are in hist and non elements
createRow :: [[Integer]] -> Integer -> Integer -> String
createRow ((h : hs) : []) row col
        | col < h = " " ++ (createRow ((h : hs) : []) row (col + 1))
        | otherwise = case getHead row (h : hs) of
                      [] -> " "
                      _ -> "*"

createRow ((h : hs) : hist) row col
        | col < h = " " ++ (createRow ((h : hs) : hist) row (col + 1))
        | otherwise = case getHead row (h : hs) of
                      [] -> " " ++ (createRow hist row (col + 1))
                      _ -> "*" ++ (createRow hist row (col + 1))

getHead :: Integer -> [Integer] -> [Integer]
getHead d es = fromIntegral d `drop` es

createMany :: String -> Integer -> String
createMany elt n = foldl (\a -> \_ -> a ++ elt) ("") [0..n]

groupByInt :: [Integer] -> [[Integer]]
groupByInt ns = groupBy (==) $ sortBy compare ns

getMaxHeight :: [[Integer]] -> Integer
getMaxHeight nns = toInteger $ foldl (\a -> \b -> max a $ length b) (0) nns

getMaxWidth :: [[Integer]] -> Integer
getMaxWidth hist = head $ last hist

formatNumbers :: Integer -> String
formatNumbers maxWidth = foldl (\a -> \b -> a ++ show b) ("") [0..maxWidth]
