{-# LANGUAGE OverloadedStrings #-}
module AdventOfCode2022 where

import qualified Data.Bool as B
import qualified Data.Char as C
import           Data.Foldable (foldlM)
import qualified Data.List as L
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, listToMaybe, mapMaybe, maybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Text.Read (readMaybe)

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

--------------------------------------------------------------------------------
  -- Day 05
--------------------------------------------------------------------------------
day05 :: IO ()
day05 = do
  lines  <- L.lines <$> readFile "data/2022/day05.txt"
  shifts <- mapM mkMove $ L.words <$> L.drop 10 lines
  cargo  <-   mkInitialCargo
            . fmap (mapMaybe (listToMaybe . L.filter (C.isAlpha)))
            . L.transpose
            . fmap (Split.chunksOf 4)
            $ L.take 8 lines

  -- Part 1
  shiftedCargo <- foldlM (shiftCargo CrateMover9000) cargo shifts
  putStrLn . show . mapMaybe listToMaybe $ Map.elems shiftedCargo

  -- Part 2
  shiftedCargo' <- foldlM (shiftCargo CrateMover9001) cargo shifts
  putStrLn . show . mapMaybe listToMaybe $ Map.elems shiftedCargo'

type Shift = (Int, Int, Int) -- Move X from Y to Z

type CargoMap = Map.Map Int String

data CraneModel
  = CrateMover9000
  | CrateMover9001
  deriving (Eq)

mkMove :: MonadFail m => [String] -> m Shift
mkMove (_:x:_:y:_:z:[]) = pure (read x, read y, read z)
mkMove line = fail $ "Could not read moves for line: " <> show line

mkInitialCargo :: MonadFail m => [String] -> m CargoMap
mkInitialCargo cargo =
  if L.length cargo == 9
     then pure . Map.fromList $ zip [1..] cargo
     else fail $ "Cargo only has " <> show (L.length cargo) <> " stacks"

shiftCargo :: MonadFail m => CraneModel -> CargoMap -> Shift -> m CargoMap
shiftCargo crane cargo (move, from, to) = do
  toShift   <- maybe (fail "No element for to value") pure $ Map.lookup to cargo
  fromShift <-
    maybe (fail "No element for shift value") pure $ Map.lookup from cargo

  let stackingOrder = B.bool id reverse $ crane == CrateMover9000
      stackCargo toStack =
        Just $ (stackingOrder $ L.take move fromShift) <> toStack

  pure . Map.update (Just . L.drop move) from $ Map.update stackCargo to cargo

--------------------------------------------------------------------------------
  -- Day 06
--------------------------------------------------------------------------------
day06 :: IO ()
day06 = do
  stream <- readFile "data/2022/day06.txt"

  -- Part 1
  putStrLn . show $ findMarker 4 4 stream

  -- Part 2
  putStrLn . show $ findMarker 14 14 stream

findMarker :: Int -> Int -> String -> Int
findMarker size pos stream
  | (== size) . Set.size . Set.fromList $ L.take size stream = pos
  | otherwise = findMarker size (pos + 1) (L.drop 1 stream)

--------------------------------------------------------------------------------
  -- Day 07
--------------------------------------------------------------------------------
day07 :: IO ()
day07 = do
  lines <- T.lines . T.pack <$> readFile "data/2022/day07.txt"
  cmds  <- catMaybes <$> mapM mkFileLine lines
  testCmds <- catMaybes <$> mapM mkFileLine testData

  let fileSizeMap = mkFileSizeMap Map.empty [] cmds
      testMap = mkFileSizeMap Map.empty [] testCmds

  -- Part 1
  putStrLn . show . sum . L.filter (100000 >) $ Map.elems fileSizeMap
  putStrLn $ show . sum . L.filter (100000 >) $ Map.elems testMap

data Command
  = CD_In T.Text
  | CD_Out
  | Dir T.Text
  | File Int

mkFileLine :: MonadFail m => T.Text -> m (Maybe Command)
mkFileLine txt =
  case T.words txt of
    (a:b:c:[]) | a == "$", b == "cd" ->
      pure . Just . B.bool (CD_In c) CD_Out $ c == ".."

    (a:b:[])
      | a == "$"   -> pure Nothing
      | a == "dir" -> pure . Just $ Dir b
      | otherwise  ->
        maybe (fail $ "Could not read file " <> T.unpack b)
              (pure . Just . File)
              (readMaybe $ T.unpack a)

    _ -> fail $ "Unexpected elements in command line"

type FileSizeMap = Map.Map T.Text Int
type CommandStack = [Command]

mkFileSizeMap :: FileSizeMap -> CommandStack -> [Command] -> FileSizeMap
mkFileSizeMap dirMap stack [] =
  fst . collapseStack 0 dirMap $ stack <> [ CD_In "/" ]

mkFileSizeMap dirMap stack (cmd:cmds) =
  case cmd of
    CD_Out ->
      let (newDirMap, newStack) = collapseStack 0 dirMap stack
       in mkFileSizeMap newDirMap newStack cmds

    _ -> mkFileSizeMap dirMap (cmd : stack) cmds

collapseStack :: Int -> FileSizeMap -> CommandStack -> (FileSizeMap, CommandStack)
collapseStack count dirMap []         = (Map.insert "/" count dirMap, [])
collapseStack count dirMap (cmd:cmds) =
  case cmd of
    CD_Out   -> collapseStack count dirMap cmds -- This should never happen
    CD_In  d -> (Map.insert d count dirMap, cmds)
    File   s -> collapseStack (count + s) dirMap cmds
    Dir    d ->
      collapseStack (count + Map.findWithDefault 0 d dirMap) dirMap cmds

testData :: [T.Text]
testData =
  [ "$ cd /"
  , "$ ls"
  , "dir a"
  , "14848514 b.txt"
  , "8504156 c.dat"
  , "dir d"
  , "$ cd a"
  , "$ ls"
  , "dir e"
  , "29116 f"
  , "2557 g"
  , "62596 h.lst"
  , "$ cd e"
  , "$ ls"
  , "584 i"
  , "$ cd .."
  , "$ cd .."
  , "$ cd d"
  , "$ ls"
  , "4060174 j"
  , "8033020 d.log"
  , "5626152 d.ext"
  , "7214296 k"
  ]

--------------------------------------------------------------------------------
  -- Day 08
--------------------------------------------------------------------------------
day08 :: IO ()
day08 = do
  lines <- L.lines <$> readFile "data/2022/day08.txt"

  let rows = fmap C.digitToInt <$> testGrid
      columns = fmap C.digitToInt <$> L.transpose testGrid
      paired = (\row -> (row, columns)) <$> rows

  putStrLn $ show paired

testGrid :: [String]
testGrid =
  [ "30373"
  , "25512"
  , "65332"
  , "33549"
  , "35390"
  ]
