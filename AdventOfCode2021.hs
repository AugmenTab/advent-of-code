module AdventOfCode2021 where

import Data.List.Split (splitOn)

day01 :: IO ()
day01 = do
  fileData <- readFile "data/2021/day01.txt"
  let depths    = map read $ lines fileData
      solution1 = depthIncreases depths
      solution2 = depthWindows depths

  putStrLn $ "Part 1: " <> show solution1
  putStrLn $ "Part 2: " <> show solution2

depthIncreases :: [Int] -> Int
depthIncreases xs = length $ filter (\(f,s) -> f < s) $ zip xs $ tail xs

depthWindows :: [Int] -> Int
depthWindows xs = depthIncreases windows
  where windows = zipWith3 (\x y z -> x + y + z) xs (tail xs) (tail $ tail xs)

day02 :: IO ()
day02 = do
  fileData <- readFile "data/2021/day02.txt"
  let moves = map (splitOn " ") $ lines fileData
      solution1 = calculatePosition (0, 0) moves
      solution2 = complexPosition (0, 0, 0) moves

  putStrLn $ "Part 1: " <> show solution1
  putStrLn $ "Part 2: " <> show solution2

calculatePosition :: (Int, Int) -> [[String]] -> Int
calculatePosition (h, d) [] = h * d
calculatePosition (h, d) ((dir:num:[]):xs)
  | dir == "up"      = calculatePosition (h, d - numInt) xs
  | dir == "down"    = calculatePosition (h, d + numInt) xs
  | dir == "forward" = calculatePosition (h + numInt, d) xs
  | otherwise        = calculatePosition (h, d) xs
  where numInt = read num

complexPosition :: (Int, Int, Int) -> [[String]] -> Int
complexPosition (h, d, _) [] = h * d
complexPosition (h, d, a) ((dir:num:[]):xs)
  | dir == "up"      = complexPosition (h, d, a - numInt) xs
  | dir == "down"    = complexPosition (h, d, a + numInt) xs
  | dir == "forward" = complexPosition (h + numInt, d + (a * numInt), a) xs
  | otherwise        = complexPosition (h, d, a) xs
  where numInt = read num
