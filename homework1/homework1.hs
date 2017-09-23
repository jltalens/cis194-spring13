toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = snd $ foldr f (False, []) xs
  where f = \v acc ->
          if fst acc == False
          then
            (True, v:(snd acc))
          else
            (False, v*2:(snd acc))


sumDigits :: [Integer] -> Integer
sumDigits xs = foldl sumDigitsInd 0 xs

sumDigitsInd :: Integer -> Integer -> Integer
sumDigitsInd acc n
  | n `div` 10 == 0 = acc + n
  | otherwise = sumDigitsInd (acc + n `mod` 10) (n `div` 10)


validate :: Integer -> Bool
validate n = (f n) `mod` 10 == 0
  where f = sumDigits . doubleEveryOther . toDigits


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | otherwise = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) b c a
