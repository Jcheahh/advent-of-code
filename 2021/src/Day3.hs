module Day3 where

import Data.List

main :: IO ()
main = do
  input <- readFile "2021/input/day3.txt"
  let inputs = lines input
      inputs' = transpose inputs
  putStrLn "Part 1:"
  print $ powerConsumption inputs'
  putStrLn "Part 2:"
  print $ powerConsumption inputs'

  case parseBits $ lines input of
    Nothing -> error "error"
    Just xs -> do
      putStrLn "Testing123"
      print $ oxygen xs * cO2 xs
      putStrLn "Testing123"
      print $ cO2 xs

parseBits :: [String] -> Maybe [[Bit]]
parseBits xs = sequence . sequence $ (mapM . mapM) parseBit xs

parseBit :: Char -> Maybe Bit
parseBit '0' = Just Zero
parseBit '1' = Just One
parseBit _ = Nothing

binToDec :: [Bit] -> Int
binToDec xs = sum $ zipWith go (reverse xs) [0 ..]
  where
    go Zero _ = 0
    go One i = 2 ^ i

data Bit = Zero | One deriving (Eq, Ord, Show)

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

zeroOneCountHead :: [[Bit]] -> (Int, Int)
zeroOneCountHead xs = go xs (0, 0)
  where
    go [] x = x
    go (x : xs) (a, b) = case head x of
      Zero -> go xs (a + 1, b)
      One -> go xs (a, b + 1)

mostCommonHead :: [[Bit]] -> Bit
mostCommonHead xs =
  let (a, b) = zeroOneCountHead xs
   in if a > b then Zero else One

leastCommonHead :: [[Bit]] -> Bit
leastCommonHead xs
  | a == 0 && b == 0 = error ":)"
  | a == 0 = One
  | b == 0 = Zero
  | otherwise = if a > b then One else Zero
  where
    (a, b) = zeroOneCountHead xs

oxygen :: [[Bit]] -> Int
oxygen = generalFunction mostCommonHead

cO2 :: [[Bit]] -> Int
cO2 = generalFunction leastCommonHead

generalFunction :: ([[Bit]] -> Bit) -> [[Bit]] -> Int
generalFunction f xs = binToDec $ go xs []
  where
    go xs@((_ : _) : _) acc =
      let b = f xs
       in go (fmap tail $ filter (\x -> head x == b) xs) (b : acc)
    go _ acc = reverse acc
