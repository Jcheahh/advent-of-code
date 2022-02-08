module Day1 (main) where

main :: IO ()
main = do
  input <- readFile "2020/input/day1.txt"
  let inputs = read <$> lines input :: [Int]
  print $ part1 inputs

part1 :: [Int] -> Int
part1 xs = go xs []
  where
    sumOf2020 x = 2020 - x
    go (x : xs) map = if elem (sumOf2020 x) map then (sumOf2020 x) * x else go xs (x : map)