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

  putStrLn $ show $ calculatePosition (0, 0) moves

calculatePosition :: (Int, Int) -> [[String]] -> Int
calculatePosition (h, d) [] = h * d
calculatePosition (h, d) ((dir:num:[]):xs)
  | dir == "up"      = calculatePosition (h, d - (read num)) xs
  | dir == "down"    = calculatePosition (h, d + (read num)) xs
  | dir == "forward" = calculatePosition (h + (read num), d) xs
  | otherwise        = calculatePosition (h, d) xs
