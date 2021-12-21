module Day1 where

import Prelude

main :: IO ()
main = do
  input <- readFile "input/day1.txt"

  let inputs = read <$> lines input
  putStrLn "Answer:"
  print $ counting inputs

counting :: [Int] -> Int
counting xs = go xs 0 0
  where
    go [] _ count = count
    go (x : xs) 0 count = go xs x count
    go (x : xs) acc count = if x > acc then go xs x count + 1 else go xs x count
