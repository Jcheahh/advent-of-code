module Day7 where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- readFile "year2020/input/day7.txt"
  let inputs = lines input
      inputs' = fmap words inputs
      answer = Set.size $ part1 ((parentMap . func2 . parseInput) inputs') (Bag "shiny" "gold")
      answer2 = (part2 . func2 . parseInput) inputs' (Bag "shiny" "gold")
  print "Part 1"
  print answer
  print "Part 2"
  print answer2

type Color = String

type ColorType = String

type Amount = Int

data Bag = Bag ColorType Color deriving (Ord, Eq, Show)

data Luggage = Luggage Amount Bag deriving (Ord, Eq, Show)

func2 :: [(Bag, [Luggage])] -> Map.Map Bag [Luggage]
func2 = Map.fromList

parseInput :: [[String]] -> [(Bag, [Luggage])]
parseInput =
  map
    ( \x ->
        if length x == 7
          then (parseBag (take 2 x), [])
          else (parseBag (take 2 x), parseLuggage (drop 4 x))
    )

parseBag :: [String] -> Bag
parseBag x = case x of
  [c, s] -> Bag c s
  _ -> error "Input error"

parseSMT :: [String] -> Luggage
parseSMT [a, b, c] = Luggage (read a) (Bag b c)
parseSMT _ = error "Invalid input"

parseLuggage :: [String] -> [Luggage]
parseLuggage [] = []
parseLuggage x = parseSMT (take 3 x) : parseLuggage (drop 4 x)

insertParents :: Bag -> [Luggage] -> Map.Map Bag [Bag] -> Map.Map Bag [Bag]
insertParents _ [] emptyMap = emptyMap
insertParents p ((Luggage n c) : ncs) emptyMap =
  Map.insertWith (++) c [p] (insertParents p ncs emptyMap)

parentMap :: Map.Map Bag [Luggage] -> Map.Map Bag [Bag]
parentMap = Map.foldrWithKey insertParents Map.empty

part1 :: Map.Map Bag [Bag] -> Bag -> Set.Set Bag
part1 map color = case Map.lookup color map of
  Nothing -> Set.empty
  Just bags ->
    let parent = Set.fromList bags
        xs = foldr (Set.union . part1 map) Set.empty bags
     in Set.union parent xs

part2 :: Map.Map Bag [Luggage] -> Bag -> Int
part2 map color = case Map.lookup color map of
  Nothing -> 0
  Just [] -> 0
  Just bags ->
    let parent = foldr (\(Luggage count bags) a -> count + a) 0 bags
        xs = foldr (\(Luggage count bag) acc -> (count * part2 map bag) + acc) 0 bags
     in parent + xs
