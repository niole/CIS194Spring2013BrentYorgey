{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

letterScores :: [(Char, Int)]
letterScores = zip ['a'..'z'] [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]
  ++ zip ['A'..'Z'] [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

newtype Score = Score Int
        deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

--scores according to letterScores
score :: Char -> Score
score = helper letterScores
        where helper [] _ = 0
              helper ((chr, s) : scores) c
                | chr == c = Score s
                | otherwise = helper scores c

--looks up every letter every time, regardless of uniqueness
scoreString :: String -> Score
scoreString str = Score $ helper str
                where helper [] = 0
                      helper (s:ss) = case score s of
                                      Score n -> n + helper ss
