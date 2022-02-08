module Day4 where

import Data.Char

main :: IO ()
main = do
  input <- readFile "input/day4.txt"
  board <- readFile "input/day4board.txt"
  let inputs = read <$> lines input :: [Int]
      boards = replaceSpace' $ splitWithMtyString $ lines board

  -- read <$> words boards :: [Int]
  -- print inputs -- :: (Read a, Show a) => a
  print boards -- :: (Read a, Show a) => a

splitWithMtyString :: [String] -> [[String]]
splitWithMtyString xs = go xs [] []
  where
    go [] _ bacc = bacc
    go (x : xs) acc bacc = if x /= "" then go xs (x : acc) bacc else go xs [] (acc : bacc)

replaceSpace :: String -> [Int]
replaceSpace xs = read <$> words xs

replaceSpace' :: [[String]] -> [[[Int]]]
replaceSpace' xs = go xs []
  where
    go [] acc = acc
    go (x : xs) acc = go xs (fmap replaceSpace x : acc)

--       inputs' = transpose inputs
--   putStrLn "Part 1:"
--   print $ powerConsumption inputs'
--   putStrLn "Part 2:"
--   print $ powerConsumption inputs'

--   case parseBits $ lines input of
--     Nothing -> error "error"
--     Just xs -> do
--       putStrLn "Testing123"
--       print $ oxygen xs * cO2 xs
--       putStrLn "Testing123"
--       print $ cO2 xs

bingo :: [Int] -> [[[Int]]] -> Int
bingo = undefined