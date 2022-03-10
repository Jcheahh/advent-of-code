module Day8 where

import qualified Data.Map as Map

main :: IO ()
main = do
  input <- readFile "year2020/input/day8.txt"
  let inputs = (zipIns . parseIns . lines) input
  print "Part 1"
  -- 613
  print $ part1 inputs

-- print inputs

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
