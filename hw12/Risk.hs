{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List
import Debug.Trace

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

concatRolls :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen [DieValue]
concatRolls a b = do
        adv <- a
        bdv <- b
        return $ adv ++ bdv

makeRolls :: Int -> Rand StdGen [DieValue]
makeRolls 0 = return []
makeRolls n = concatRolls ((\dv -> [dv]) <$> die) $ makeRolls (n-1)

descSortDieRolls :: Rand StdGen [DieValue] -> Rand StdGen [DieValue]
descSortDieRolls rolls = rolls >>= (\rs -> return (sortBy (\a -> \b -> compare (unDV b) (unDV a)) rs))

pairBattles :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen [(DieValue, DieValue)]
pairBattles as bs = as >>=
                        \ars -> bs >>=
                          \brs ->
                            return $ zip ars brs

getSubbattles :: Battlefield -> Rand StdGen [(DieValue, DieValue)]
getSubbattles b = let as = attackers b
                      ds = defenders b
                      attackerRolls = descSortDieRolls $ makeRolls $ as - 1
                      defenderRolls = descSortDieRolls $ makeRolls $ min ds 2
                  in pairBattles attackerRolls defenderRolls

getLosses :: Rand StdGen [(DieValue, DieValue)] -> (Battlefield -> Rand StdGen Battlefield)
getLosses subbs = f
                  where f b = let ls = aggregateLosses <$> subbs
                              in (\ls -> Battlefield ((attackers b) - (fst ls)) ((defenders b) - (snd ls))) <$> ls
                        aggregateLosses = foldl (\ls -> \next -> updateLosses next ls) (0, 0)
                        updateLosses (a, d) (al, dl)
                                | a > d = (al, dl + 1)
                                | a < d = (al + 1, dl)
                                | otherwise = (al, dl)

-- E2

battle :: Battlefield -> Rand StdGen Battlefield
battle b = getLosses (getSubbattles b) b

-- E3

invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
  Battlefield a d <- battle b
  invadeNext a d
        where invadeNext a d
                | a < 2 || d <= 0 = return $ Battlefield a d
                |otherwise = invade $ Battlefield a d
