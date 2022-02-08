module Day1 (main) where

main :: IO ()
main = do
  input <- readFile "2020/input/day1.txt"
  let inputs = read <$> lines input :: [Int]
  print "Part 1"
  print $ part1 inputs
  print "Part 2"
  print $ part2 inputs

part1 :: [Int] -> Int
part1 xs = case searchTwoSumTo 2020 xs of
  Nothing -> 0
  Just x -> x

part2 :: [Int] -> Int
part2 xs = go xs
  where
    sumOf2020 x = 2020 - x
    go (y : ys) = case searchTwoSumTo (sumOf2020 y) ys of
      Nothing -> go ys
      Just c -> y * c

searchTwoSumTo :: Int -> [Int] -> Maybe Int
searchTwoSumTo n xs = go xs []
  where
    sumOf x = n - x
    go [] _ = Nothing
    go (x : xs) map =
      if elem (sumOf x) map
        then Just $ (sumOf x) * x
        else go xs (x : map)
