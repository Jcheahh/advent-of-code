module Day1 where

import Prelude

main :: IO ()
main = do
  input <- readFile "2021/input/day1.txt"

  let inputs = read <$> lines input
  putStrLn "Part 1:"
  print $ part1 inputs
  putStrLn "Part 2:"
  print $ part2 inputs

part1 :: [Int] -> Int
part1 xs = go xs 0 0
  where
    go [] _ count = count
    go (x : xs) 0 count = go xs x count
    go (x : xs) acc count = if x > acc then go xs x (count + 1) else go xs x count

part1' :: [Int] -> Int
part1' xs =
  length
    . filter id
    . zipWith (>) (tail xs)
    $ xs

part2 :: [Int] -> Int
part2 xs = go xs 0 0
  where
    go (x : y : z : xs) 0 count = go (y : z : xs) (x + y + z) count
    go (x : y : z : xs) acc count =
      if x + y + z > acc
        then go (y : z : xs) (x + y + z) (count + 1)
        else go (y : z : xs) (x + y + z) count
    go _ _ count = count

part2' :: [Int] -> Int
part2' xs = go xs 0
  where
    go (a : b : c : d : xs) count =
      if d > a
        then go (b : c : d : xs) $ count + 1
        else go (b : c : d : xs) count
    go _ count = count

part2'' :: [Int] -> Int
part2'' (x : y : xs) =
  part1 $
    zipWith3 (\a b c -> a + b + c) (x : y : xs) (y : xs) xs
part2'' _ = 0