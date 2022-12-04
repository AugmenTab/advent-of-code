{-# LANGUAGE OverloadedStrings #-}
module AdventOfCode2022 where

import           Data.Foldable (foldlM)
import qualified Data.List as L
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe, mapMaybe, maybe)
import qualified Data.Set as Set
import qualified Data.Text as T

--------------------------------------------------------------------------------
  -- Day 01
--------------------------------------------------------------------------------
day01 :: IO ()
day01 = do
  fileData <- readFile "data/2022/day01.txt"

  let sumCalories = sum . fmap (read . T.unpack)
      getCalories = fmap (sumCalories . T.lines) . T.splitOn "\n\n" . T.pack
      allCalories = reverse . L.sort $ getCalories fileData

  -- Part 1:
  putStrLn $ "Most calories: " <> show (listToMaybe allCalories)

  -- Part 2:
  putStrLn $ "Top 3 calories: " <> show (sum $ L.take 3 allCalories)

--------------------------------------------------------------------------------
  -- Day 02
--------------------------------------------------------------------------------
day02 :: IO ()
day02 = do
  lines <- T.lines . T.pack <$> readFile "data/2022/day02.txt"

  let indexed = fmap (T.splitOn " ") <$> L.zip [1..] lines

  -- Part 1
  matches  <- mapM mkMatch indexed
  putStrLn $ "From moves: " <> show (L.foldl' foldMatches 0 matches)

  -- Part 2
  matches' <- mapM mkMatchWithResult indexed
  putStrLn $ "From results: " <> show (L.foldl' foldMatches 0 matches')

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
    _   -> fail $ "Unrecognized Move " <> T.unpack txt <> " at line " <> show i

pickLosingMove :: Move -> Move
pickLosingMove Rock     = Scissors
pickLosingMove Paper    = Rock
pickLosingMove Scissors = Paper

pickWinningMove :: Move -> Move
pickWinningMove Rock     = Paper
pickWinningMove Paper    = Scissors
pickWinningMove Scissors = Rock

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

data Result
  = Lose
  | Draw
  | Win

resultFromText :: MonadFail m => Int -> T.Text -> m Result
resultFromText i txt =
  case txt of
    "X" -> pure Lose
    "Y" -> pure Draw
    "Z" -> pure Win
    _   -> fail $ "Unrecognized Result " <> T.unpack txt <> " at line " <> show i

mkMatch :: MonadFail m => (Int, [T.Text]) -> m Match
mkMatch (i, (f:s:[])) = do
  (,) <$> moveFromText i f <*> moveFromText i s

mkMatch (i, _) = fail $ "Bad input on line " <> show i

mkMatchWithResult :: MonadFail m => (Int, [T.Text]) -> m Match
mkMatchWithResult (i, (m:r:[])) = do
  m' <- moveFromText i m
  r' <- resultFromText i r

  let playerMove =
        case r' of
          Lose -> pickLosingMove m'
          Draw -> m'
          Win  -> pickWinningMove m'

  pure (m', playerMove)

mkMatchWithResult (i, _) = fail $ "Bad input on line " <> show i

foldMatches :: Int -> Match -> Int
foldMatches acc match@(o, p) = acc + pointsForMatch match + pointsForMove p

--------------------------------------------------------------------------------
  -- Day 03
--------------------------------------------------------------------------------
day03 :: IO ()
day03 = do
  lines <- L.lines <$> readFile "data/2022/day03.txt"

  -- Part 1
  sharedItems <- foldlM foldPriority 0 $ findSharedItem <$> lines
  putStrLn $ show sharedItems

  -- Part 2
  badges <-
    foldlM foldPriority 0 $ findBadgeItem <$> Split.chunksOf 3 lines
  putStrLn $ show badges

foldPriority :: MonadFail m => Int -> Maybe Char -> m Int
foldPriority acc item = do
    maybe (fail "Could not determine priority") (pure . (+) acc)
  $ flip Map.lookup priorityMap =<< item

findSharedItem :: String -> Maybe Char
findSharedItem str =
  let half = div (L.length str) 2
      firstPack = Set.fromList $ L.take half str
      secondPack = Set.fromList $ L.drop half str
   in listToMaybe . Set.toList $ Set.intersection firstPack secondPack

findBadgeItem :: [String] -> Maybe Char
findBadgeItem packs =
  if L.length packs /= 3
     then Nothing
     else   listToMaybe
          . Set.toList
          . L.foldl1' Set.intersection
          $ Set.fromList <$> packs

priorityMap :: Map.Map Char Int
priorityMap = Map.fromList . flip L.zip [1..] $ ['a'..'z'] <> ['A'..'Z']

--------------------------------------------------------------------------------
  -- Day 04
--------------------------------------------------------------------------------
day04 :: IO ()
day04 = do
  lines <- T.lines . T.pack <$> readFile "data/2022/day04.txt"
  pairedRanges <- mapM pairRanges $ T.split (flip elem [ '-', ',' ]) <$> lines

  -- Part 1
  putStrLn . show $ L.foldl' foldContaining 0 pairedRanges

  -- Part 2
  putStrLn . show $ L.foldl' foldOverlapping 0 pairedRanges

pairRanges :: MonadFail m => [T.Text] -> m ([Int], [Int])
pairRanges (a:b:c:d:[]) =
  pure ( [ read (T.unpack a)..read (T.unpack b) ]
       , [ read (T.unpack c)..read (T.unpack d) ]
       )

pairRanges txt =
  fail $ "Unexpected number of elements in assignment: " <> show txt

foldContaining :: Int -> ([Int], [Int]) -> Int
foldContaining acc (a1, a2)
  | L.isInfixOf a1 a2 || L.isInfixOf a2 a1 = acc + 1
  | otherwise                              = acc

foldOverlapping :: Int -> ([Int], [Int]) -> Int
foldOverlapping acc (a1, a2)
  | null (L.intersect a1 a2) = acc
  | otherwise                = acc + 1
