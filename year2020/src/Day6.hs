module Day6 where

import Data.List (intersect)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- readFile "year2020/input/day6.txt"
  let inputs = lines input
  let inputs' = fmap concat $ splitWithMtyString inputs
  print "Part 1"
  print $ sumOfCount inputs'
  print "Part 2"
  print $ sum $ fmap checkDuplicate $ splitWithMtyString inputs

splitWithMtyString :: [String] -> [[String]]
splitWithMtyString xs = go xs [] []
  where
    go [] _ bacc = bacc
    go (x : xs) acc bacc = if x /= "" then go xs (x : acc) bacc else go xs [] (acc : bacc)

sumOfCount :: [String] -> Int
sumOfCount = foldr (\x a -> a + length (Set.fromList x)) 0

checkYes :: [String] -> Int
checkYes xs = go xs Set.empty
  where
    go xs set = undefined

checkDuplicate :: [String] -> Int
checkDuplicate [] = 0
checkDuplicate (x : xs) = length $ foldr intersect x xs
