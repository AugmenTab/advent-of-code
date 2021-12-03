module AdventOfCode2021 where

import Data.List       (transpose)
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

day03 :: IO ()
day03 = do
  fileData <- readFile "data/2021/day03.txt"
  let binaries  = lines fileData
      solution1 = findPowerConsumption (transpose binaries)

  putStrLn $ "Part 1: " <> show solution1

findPowerConsumption :: [String] -> Int
findPowerConsumption bs = (binToInt $ reverse gamma) * (binToInt $ reverse epsilon)
  where mkPairs = foldr (\x (a, b) -> if x == '0'
                                         then (a + 1, b)
                                         else (a, b + 1))
                        (0,0)
        pairs   = map mkPairs bs
        gamma   = map (\(x, y) -> if x > y then 0 else 1) pairs
        epsilon = map (\(x, y) -> if x > y then 1 else 0) pairs

binToInt :: [Int] -> Int
binToInt bin = go bin 0
  where go []     _ = 0
        go (x:xs) n = (x * (2^n)) + (go xs (n + 1))
