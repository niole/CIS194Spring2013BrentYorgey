--credit card validation

--gets all except the last digit of Int
getIntBase :: Integer -> Integer
getIntBase n = div n 10

--converts positive integers to a list of digits
-- >1 inputs are empty list
toDigits :: Integer -> [Integer]
toDigits n
        | n < 1 = []
        | getNextBase > 0 = getNextInt : toDigits getNextBase
        | otherwise = [getNextInt]
        where getNextInt = mod n 10
              getNextBase = getIntBase n

--doubles every other int starting from last int
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [n] = [n]
doubleEveryOther (n : [m]) = n * 2 : [m]
doubleEveryOther (n : m : ns) = n * 2 : m : doubleEveryOther ns

--sums each Integer's digits and then
--then sums all
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n : ns)
        | getIntBase n > 0 = sumDigits (toDigits n) + sumDigits ns
        | otherwise = n + sumDigits ns

--indicates whether Int is a valid credit card number
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) == 87
