{-# LANGUAGE OverloadedStrings #-}
module AdventOfCode2022 where

import qualified Data.List as L
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T

day01 :: IO ()
day01 = do
  let sumCalories = sum . fmap (read . T.unpack)
      getCalories = fmap (sumCalories . T.lines) . T.splitOn "\n\n" . T.pack

  fileData <- readFile "data/2022/day01.txt"
  putStrLn . show . listToMaybe . reverse . L.sort $ getCalories fileData
