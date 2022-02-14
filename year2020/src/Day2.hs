{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Char
import Text.Trifecta

main :: IO ()
main = do
  input <- readFile "year2020/input/day2.txt"
  let inputs = parseString parsePasswordPolicyes mempty input
  putStrLn "Part 1"
  case inputs of
    Success pps -> do
      print $ validPasswords pps
      putStrLn "Part2"
      print $ validPasswords' pps
    Failure ei -> print "Input invalid"

--   print "Part 2"
--   print $ part2 inputs

type Range = (Integer, Integer)

type Letter = Char

type Password = String

data PasswordPolicy = PasswordPolicy Range Letter Password deriving (Show)

parsePasswordPolicyes :: Parser [PasswordPolicy]
parsePasswordPolicyes = many $ parsePasswordPolicy <* char '\n'

parsePasswordPolicy :: Parser PasswordPolicy
parsePasswordPolicy = do
  range <- parseRange

  char ' '

  letter <- parseLetter

  char ':'

  char ' '

  PasswordPolicy range letter <$> parsePassword

parseRange :: Parser Range
parseRange = do
  low <- decimal

  char '-'

  high <- decimal

  return (low, high)

parseLetter :: Parser Letter
parseLetter = do
  letter

parsePassword :: Parser Password
parsePassword = do
  some letter

validPassword :: PasswordPolicy -> Bool
validPassword (PasswordPolicy (low, high) letter password) = go letter password 0
  where
    go _ [] count = count >= low && count <= high
    go letter (x : xs) count =
      if x == letter
        then go letter xs (count + 1)
        else go letter xs count

validPassword' :: PasswordPolicy -> Bool
validPassword' (PasswordPolicy (low, high) letter password) = go letter password 1 ' ' ' '
  where
    go letter [] index char1 char2 = (char1 == letter) /= (char2 == letter)
    go letter (x : xs) index char1 char2
      | index == low = go letter xs (index + 1) x char2
      | index == high = go letter xs (index + 1) char1 x
      | otherwise = go letter xs (index + 1) char1 char2

validPasswords :: [PasswordPolicy] -> Int
validPasswords =
  foldr
    ( \x acc ->
        if validPassword x
          then acc + 1
          else acc
    )
    0

validPasswords' :: [PasswordPolicy] -> Int
validPasswords' =
  foldr
    ( \x acc ->
        if validPassword' x
          then acc + 1
          else acc
    )
    0
