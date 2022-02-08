module Day2 where

import Text.Read (readMaybe)

main :: IO ()
main = do
  input <- readFile "2021/input/day2.txt"

  let inputs = parseCommands $ lines input

  case inputs of
    Nothing -> error "error"
    Just xs -> do
      putStrLn "Part 1:"
      print $ dive xs
      print $ dive' xs
      putStrLn "Part 2:"
      print $ diveAim xs

data Command = Forward Int | Down Int | Up Int deriving (Eq)

parseCommands :: [String] -> Maybe [Command]
parseCommands = mapM parseCommand

parseCommand :: String -> Maybe Command
parseCommand xs = case words xs of
  -- [] -> Nothing
  x : y : [] -> case x of
    "forward" -> Forward <$> readMaybe y
    "down" -> Down <$> readMaybe y
    "up" -> Up <$> readMaybe y
    _ -> Nothing
  _ -> Nothing

-- part one
dive :: [Command] -> Int
dive xs = go xs 0 0
  where
    go (Forward int : xs) hori depth = go xs (hori + int) depth
    go (Down int : xs) hori depth = go xs hori (depth + int)
    go (Up int : xs) hori depth = go xs hori (depth - int)
    go [] hori depth = hori * depth

dive' :: [Command] -> Int
dive' xs = go xs 0 0
  where
    go (x : xs) hori depth = case x of
      Forward int -> go xs (hori + int) depth
      Down int -> go xs hori (depth + int)
      Up int -> go xs hori (depth - int)
    go [] hori depth = hori * depth

-- part two
diveAim :: [Command] -> Int
diveAim xs = go xs 0 0 0
  where
    go (Forward int : xs) hori aim depth = go xs (hori + int) aim (if aim == 0 then depth else (aim * int) + depth)
    go (Down int : xs) hori aim depth = go xs hori (aim + int) depth
    go (Up int : xs) hori aim depth = go xs hori (aim - int) depth
    go [] hori aim depth = hori * depth
