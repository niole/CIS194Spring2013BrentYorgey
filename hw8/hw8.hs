{-# LANGUAGE UnboxedTuples #-}  -- for unboxed tuples (# a, b #)
{-# LANGUAGE TupleSections #-}

import Employee
import GHC.Types (IO (..))
import Data.Tree

--E1
--1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es tf) = GL (e:es) $ tf + f
        where (Emp n f) = e

--2 monoid instance for GuestList

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL as f) (GL bs g) = GL (as ++ bs) $ f + g

--3

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL a b) (GL c d)
        | b > d = GL a b
        | otherwise = GL c d

--E2
--data Tree a = Node {
--  rootLabel :: a, -- label value
--  subForest :: [Tree a] -- zero or more child trees
--}
treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f z (Node { rootLabel = x, subForest = ys }) = foldl (treeFold f) (f z x) ys

--E3

-- takes boss of curr subtree and results
-- for each subtree under this boss, 1st part of tuple
-- is with boss next is best without boss
-- how do you know which Employee in the tuple is the previous boss?
-- maybe just never add the curr boss to any lists which have a sub boss
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (withBoss gls, withoutBoss gls)
  where withBoss ((_, e):es)  = moreFun (glCons boss e) $ withBoss es
        withBoss [] = GL [] 0
        withoutBoss ((e, _):es) = moreFun e $ withoutBoss es
        withoutBoss [] = GL [] 0

--E4

--returns funnest guest list
maxFun :: Tree Employee -> GuestList
maxFun tree = case getMaxPairs tree of { (x, y) -> moreFun x y }
                where getMaxPairs (Node { rootLabel = boss, subForest = emps }) = nextLevel boss $ map getMaxPairs emps

--E5

--main :: IO ()
--main = (maxFun <$> read <$> readFile "company.txt") >>= \(GL es fun) ->
--  putStrLn $ "Total fun: " ++ show fun ++ "\n " ++ show es
main = do
  (GL es fun) <- (maxFun <$> read <$> readFile "company.txt")
  putStrLn $ "Total fun: " ++ show fun ++ "\n " ++ show es
