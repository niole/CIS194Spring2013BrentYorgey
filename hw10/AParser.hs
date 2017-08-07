import Data.List.Split
import Data.Char

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

--E4

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  empty = Parser f
          where f _ = Nothing

  (<|>) pa pb = Parser parse
                where parse chr = let outA = runParser pa chr
                                  in case outA of
                                    Nothing -> let outB = runParser pb chr
                                               in case outB of
                                                    Nothing -> Nothing
                                                    _ -> outB
                                    _ -> outA


--E5

isInt :: Char -> Bool
isInt c = case c of
                '0' -> True
                '1' -> True
                '2' -> True
                '3' -> True
                '4' -> True
                '5' -> True
                '6' -> True
                '7' -> True
                '8' -> True
                '9' -> True
                _ -> False

parseFirst :: (Char -> Bool) -> Parser ()
parseFirst check = Parser f
        where f [] = Nothing
              f (x:xs)
                | check x = Just ((), xs)
                | otherwise = Nothing

parseInt :: Parser ()
parseInt = parseFirst isInt

parseUpperCase :: Parser ()
parseUpperCase = parseFirst isUpper

intOrUppercase :: Parser ()
intOrUppercase = parseUpperCase <|> parseInt
