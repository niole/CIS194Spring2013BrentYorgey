-- E1
fib :: Integer -> Integer
fib n
        | n < 2 = 1
        | otherwise = (fib $ n - 1) + (fib $ n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- E2, efficient fib

fibs2 :: [Integer]
fibs2 = 1 : doFib 0 1
  where doFib x y = x + y : (doFib y $ x+y)

-- E3
-- define a polymorphic Stream data type

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show (Cons s ss) = show s ++ show ss

-- call with self referencing Stream as argument e.g.
-- let x = Cons 5 x
-- take 5 $ streamToList $ Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 x
streamToList :: Stream a -> [a]
streamToList (Cons s ss) = s : streamToList ss

--E4

streamRepeat :: a -> Stream a
streamRepeat e = Cons e $ streamRepeat e

--example: take 3 $ streamToList $ streamMap (1+) $ Cons 1 $ Cons 2 $ streamRepeat 3
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a bs) = Cons (f a) $ streamMap f bs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f $ f a

--E5

nats :: Stream Integer
nats = streamFromSeed (1+) 0
