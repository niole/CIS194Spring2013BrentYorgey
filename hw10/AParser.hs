
import Data.List.Split

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
              where
                f [] = Nothing -- fail on the empty input
                f (x:xs) -- check if x satisfies the predicate
                  | p x = Just (x, xs)
                  | otherwise = Nothing -- otherwise, fail

--E1, implement a Functor instance of parser
instance Functor Parser where
  fmap f (Parser a) = Parser adaptor
                        where adaptor chr = case a chr of
                                                Just (x, y) -> Just(f x, y)
                                                Nothing -> Nothing

instance Applicative Parser where
  pure a = Parser adaptor
                where adaptor chr = Just (a, chr)

  (<*>) (Parser fab) (Parser a) = Parser adaptor
                                where adaptor chr = case fab chr of
                                                      Just (ab, c) -> case a chr of
                                                                        Just (x, y) -> Just (ab x, y)
                                                                        Nothing -> Nothing
                                                      Nothing -> Nothing

type Name = String
data Employee = Emp { name :: Name, phone :: String }
parseName :: Parser Name
parseName = Parser f
                where f _ = Just ("blah", "rest")

parsePhone :: Parser String
parsePhone = Parser f
                where f _ = Just ("blah", "rest")

-- E3

abParser :: Parser (Char, Char)
abParser = Parser parse
              where parse [] = Nothing
                    parse [x] = Nothing
                    parse (x:y:zs)
                      | x == 'a' && y == 'b' = Just((x,y), zs)
                      | otherwise = Nothing

abParser_ :: Parser ()
abParser_ = Parser parse
              where parse [] = Nothing
                    parse [x] = Nothing
                    parse (x:y:zs)
                      | x == 'a' && y == 'b' = Just((), zs)
                      | otherwise = Nothing

intPair :: Parser [Int]
intPair = Parser parse
              where parse ns = case splitOn " " ns of
                                        [] -> Nothing
                                        [x] -> Nothing
                                        x:y:[] -> Just(map (\n -> read n :: Int) [x, y], "")
                                        _ -> Nothing
