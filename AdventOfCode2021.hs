module AdventOfCode2021 where

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
