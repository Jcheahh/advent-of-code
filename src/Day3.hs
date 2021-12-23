module Day3 where

import Data.List

main :: IO ()
main = do
  input <- readFile "input/day3.txt"
  let inputs = lines input
      inputs' = transpose inputs
  putStrLn "Part 1:"
  print $ powerConsumption inputs'

--   print $ powerConsumption inputs'

-- bintodec :: Integral i => i -> Maybe i
-- bintodec 0 = Just 0
-- bintodec i
--   | last < 2 = fmap (\x -> 2 * x + last) (bintodec (div i 10))
--   | otherwise = Nothing
--   where
--     last = mod i 10

binToDec :: [Bit] -> Int
binToDec xs = sum $ zipWith go (reverse xs) [0 ..]
  where
    go Zero _ = 0
    go One i = 2 ^ i

data Bit = Zero | One deriving (Eq, Ord)

-- mostCommon :: [Bit] -> Bit
-- mostCommon xs = snd $ maximum [([1 | y <- xs, y == x], x) | x <- xs]

mostCommon :: [Bit] -> Bit
mostCommon xs = go xs (0, 0)
  where
    go [] (a, b) = if a > b then Zero else One
    go (x : xs) (a, b) = case x of
      Zero -> go xs (a + 1, b)
      One -> go xs (a, b + 1)

mostCommon' :: String -> Bit
mostCommon' xs = go xs (0, 0)
  where
    go [] (a, b) = if a > b then Zero else One
    go (x : xs) (a, b) = case x of
      '0' -> go xs (a + 1, b)
      '1' -> go xs (a, b + 1)
      _ -> error "error"

notBit :: Bit -> Bit
notBit Zero = One
notBit One = Zero

powerConsumption :: [String] -> Int
powerConsumption xs =
  let ys = fmap mostCommon' xs -- [Bit]
      gamma = binToDec ys
      epsilon = binToDec $ fmap notBit ys
   in gamma * epsilon
