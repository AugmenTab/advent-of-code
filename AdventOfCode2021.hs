module AdventOfCode2021 where

day01 :: IO ()
day01 = do
  fileData <- readFile "data/2021/day01.txt"
  let depths    = map read $ lines fileData
      solution1 = depthIncreases depths

  putStrLn $ "Part 1: " <> show solution1

depthIncreases :: [Int] -> Int
depthIncreases xs = length $ filter (\(f,s) -> f < s) $ zip xs $ tail xs
