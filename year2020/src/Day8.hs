module Day8 where

import qualified Data.Map as Map

main :: IO ()
main = do
  input <- readFile "year2020/input/day8.txt"
  let inputs = (zipIns . parseIns . lines) input
  print "Part 1"
  print $ part1 inputs
  print "Part 2"
  print $ part2 inputs

data Operation = Accumulator | Jumps | NoOperation deriving (Ord, Eq, Show)

type SignedNumber = String

data Instruction = Instruction Operation SignedNumber deriving (Ord, Eq, Show)

parseOper :: String -> Operation
parseOper xs
  | xs == "nop" = NoOperation
  | xs == "acc" = Accumulator
  | xs == "jmp" = Jumps
  | otherwise = error "Invalid Input"

parseIns :: [String] -> [Instruction]
parseIns = fmap (\x -> Instruction (parseOper (take 3 x)) (drop 4 x))

zipIns :: [Instruction] -> [(Instruction, Bool)]
zipIns xs = zip xs $ repeat False

setAt :: Int -> (a -> a) -> [a] -> [a]
setAt _ _ [] = []
setAt n f (x : xs)
  | n < 0 = x : xs
  | n == 0 = f x : xs
  | otherwise = x : setAt (n - 1) f xs

part1 :: [(Instruction, Bool)] -> Int
part1 xs = go xs 0 0
  where
    go xs acc index =
      if bool || index < 0
        then acc
        else case op of
          Accumulator ->
            if operator == '+'
              then go alterBool (acc + number) (index + 1)
              else go alterBool (acc - number) (index + 1)
          Jumps ->
            if operator == '+'
              then go alterBool acc (index + number)
              else go alterBool acc (index - number)
          NoOperation -> go alterBool acc (index + 1)
      where
        (Instruction op signedNumber, bool) = xs !! index
        alterBool = setAt index (\(i, b) -> if b then (i, b) else (i, not b)) xs
        operator = head signedNumber
        number = read $ tail signedNumber :: Int

changeInstruc :: [(Instruction, Bool)] -> Int -> [(Instruction, Bool)]
changeInstruc [] n = []
changeInstruc (x : xs) n
  | n < 0 = x : xs
  | n > 0 = x : changeInstruc xs (n -1)
  | otherwise = case op of
    Accumulator -> x : xs
    Jumps -> (Instruction NoOperation signedNumber, bool) : xs
    NoOperation -> (Instruction Jumps signedNumber, bool) : xs
  where
    (Instruction op signedNumber, bool) = x

part2 :: [(Instruction, Bool)] -> Int
part2 ys = go ys 0 0 0
  where
    go [] acc _ _ = acc
    go xs acc index indexToChange
      | index >= length xs = acc
      | bool || index < 0 = go (changeInstruc ys indexToChange) 0 0 (indexToChange + 1)
      | otherwise = case op of
        Accumulator ->
          if operator == '+'
            then go alterBool (acc + number) (index + 1) indexToChange
            else go alterBool (acc - number) (index + 1) indexToChange
        Jumps ->
          if operator == '+'
            then go alterBool acc (index + number) indexToChange
            else go alterBool acc (index - number) indexToChange
        NoOperation -> go alterBool acc (index + 1) indexToChange
      where
        (Instruction op signedNumber, bool) = xs !! index
        alterBool =
          setAt index (\(i, b) -> if b then (i, b) else (i, not b)) xs
        operator = head signedNumber
        number = read $ tail signedNumber :: Int
