module Day5 where

import Prelude

main :: IO ()
main = do
  input <- readFile "year2020/input/day5.txt"
  let inputs = parseInput <$> lines input
  let answer = foldr max (-1) $ map findSeat inputs
  print "Part 1"
  print answer

type F = Char

type B = Char

data FB = F | B deriving (Eq, Show)

type R = Char

type L = Char

data RL = L | R deriving (Eq, Show)

toFB :: String -> [FB]
toFB = fmap (\x -> if x == 'F' then F else B)

toRL :: String -> [RL]
toRL = fmap (\x -> if x == 'R' then R else L)

parseInput :: String -> ([FB], [RL])
parseInput xs =
  let row = toFB (take 7 xs)
      col = toRL (drop 7 xs)
   in (row, col)

mid :: Integral a => a -> a -> a
mid a b = a + div (b - a) 2

findSeat :: ([FB], [RL]) -> Int
findSeat (xs, ys) = go xs ys 0 127 0 7 0 0
  where
    go [] [] _ _ _ _ row col = row * 8 + col
    go [] ys low up left right row col = case ys of
      [] -> 0
      [x] ->
        if x == L
          then go [] [] low up left right row left
          else go [] [] low up left right row right
      L : fbs -> go [] fbs low up left (mid left right) row col
      R : fbs -> go [] fbs low up (mid left right + 1) right row col
    go xs ys low up left right row col = case xs of
      [] -> 0
      [x] ->
        if x == F
          then go [] ys low up left right low col
          else go [] ys low up left right up col
      F : fbs -> go fbs ys low (mid low up) left right row col
      B : fbs -> go fbs ys (mid low up + 1) up left right row col
