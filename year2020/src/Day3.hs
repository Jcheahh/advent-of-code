module Day3 where

main :: IO ()
main = do
  input <- readFile "year2020/input/day3.txt"
  let inputs = lines input :: [String]
  putStrLn "Part 1"
  print $ countTrees inputs (3, 1)
  putStrLn "Part 2"
  let inputs' =
        [ (1, 1),
          (3, 1),
          (5, 1),
          (7, 1),
          (1, 2)
        ]
  print $ product $ fmap (countTrees inputs) inputs'
  print $ countTrees' inputs 3 1

data Tree = Square | Tree

isTree :: Tree -> Bool
isTree Tree = True
isTree Square = False

drops :: Int -> [String] -> [String]
drops d xs =
  let ys = drop d xs
   in if null ys
        then []
        else head ys : drops d ys

countTrees :: [String] -> (Int, Int) -> Int
countTrees xs (right, down) =
  length . filter isTree
    . fmap
      ( \(x, i) ->
          if (x !! (i `mod` length x))
            == '.'
            then Square
            else Tree
      )
    $ zip
      (drops down xs)
      [right, right + right ..]

countTrees' :: [String] -> Int -> Int -> Int
countTrees' xs right down =
  length $
    filter (== '#') $
      zipWith
        ( \x i ->
            x !! (i `mod` length x)
        )
        (drops down xs)
        [right, right + right ..]
