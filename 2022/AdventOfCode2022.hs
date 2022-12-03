{-# LANGUAGE OverloadedStrings #-}
module AdventOfCode2022 where

import qualified Data.List as L
import           Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T

day01 :: IO ()
day01 = do
  fileData <- readFile "data/2022/day01.txt"

  let sumCalories = sum . fmap (read . T.unpack)
      getCalories = fmap (sumCalories . T.lines) . T.splitOn "\n\n" . T.pack
      allCalories = reverse . L.sort $ getCalories fileData

  putStrLn $ "Most calories: " <> show (listToMaybe allCalories)
  putStrLn $ "Top 3 calories: " <> show (sum $ L.take 3 allCalories)

data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show)

instance Ord Move where
  compare Rock     Rock     = EQ
  compare Rock     Paper    = LT
  compare Rock     Scissors = GT
  compare Paper    Rock     = GT
  compare Paper    Paper    = EQ
  compare Paper    Scissors = LT
  compare Scissors Rock     = LT
  compare Scissors Paper    = GT
  compare Scissors Scissors = EQ

moveFromText :: MonadFail m => Int -> T.Text -> m Move
moveFromText i txt =
  case txt of
    "A" -> pure Rock
    "B" -> pure Paper
    "C" -> pure Scissors
    "X" -> pure Rock
    "Y" -> pure Paper
    "Z" -> pure Scissors
    _   -> fail $ "Unrecognized Move " <> T.unpack txt <> "at line " <> show i

pointsForMove :: Move -> Int
pointsForMove Rock     = 1
pointsForMove Paper    = 2
pointsForMove Scissors = 3

type Match = (Move, Move)

pointsForMatch :: Match -> Int
pointsForMatch (o, p) =
  case compare o p of
    GT -> 0
    EQ -> 3
    LT -> 6

mkMatch :: MonadFail m => (Int, [T.Text]) -> m Match
mkMatch (i, (f:s:[])) = do
  f <- moveFromText i f
  s <- moveFromText i s
  pure (f, s)

mkMatch (i, _) = fail $ "Bad input on line " <> show i

foldMatches :: Int -> Match -> Int
foldMatches acc match@(o, p) = acc + pointsForMatch match + pointsForMove p

day02 :: IO ()
day02 = do
  lines <- T.lines . T.pack <$> readFile "data/2022/day02.txt"
  matches <- mapM (mkMatch . fmap (T.splitOn " ")) $ L.zip [1..] lines

  putStrLn . show $ L.foldl' foldMatches 0 matches
