module Levels where


import qualified Data.List.Split as Split
import qualified System.Random   as Random


-- | Pipe that connects sides specified by True
-- | sides are specified in order Up, Right, Left, Down
data Pipe
  = ConnectivePipe   -- ^ Pipes connecting several sides, open sides are marked as True
  {
    up    :: Bool,
    right :: Bool,
    down  :: Bool,
    left  :: Bool
  }
  | SourcePipe       -- ^ Horizontal source pipe (water flows from left to right)
  | DestinationPipe  -- ^ Horizontal destination pipe (water flows from left to right)
  deriving (Show, Eq)

-- | A cell is either empty or has pipe in it
-- | Some Cells are meant to be empty by designers of the level
type Cell = Maybe Pipe

-- | A cell which can be either empty or filled with water
data FlowCell
  = FilledCell Cell Bool
  | EmptyCell Cell
  deriving (Show, Eq)

-- | A level is a 2d grid of cells
type Level = [[Cell]]

-- | A level with information about filled water
type FlowLevel = [[FlowCell]]

-- | Make FlowLevel from Level
levelToFlowLevel :: Level -> FlowLevel
levelToFlowLevel = map (map EmptyCell)

-- | Convert string representation of level to level type
stringsToLevel :: [String] -> Level
stringsToLevel = map (map strToCell . Split.splitOn " ")
  where
    strToCell :: String -> Cell
    strToCell c =
      case c of
        "╼" -> Just SourcePipe
        "╾" -> Just DestinationPipe
        "┃" -> Just (ConnectivePipe True False True False)
        "━" -> Just (ConnectivePipe False True False True)
        "┏" -> Just (ConnectivePipe False True True False)
        "┓" -> Just (ConnectivePipe False False True True)
        "┗" -> Just (ConnectivePipe True True False False)
        "┛" -> Just (ConnectivePipe True False False True)
        "┣" -> Just (ConnectivePipe True True True False)
        "┫" -> Just (ConnectivePipe True False True True)
        "┳" -> Just (ConnectivePipe False True True True)
        "┻" -> Just (ConnectivePipe True True False True)
        "╋" -> Just (ConnectivePipe True True True True)
        _   -> Nothing


-- | Symbol representation for level1
-- | Character set for pipes: https://unicode-table.com/en/blocks/box-drawing/
reprOfLevel1 :: [String]
reprOfLevel1 =
  [ "╼ ┓ ┏ ┳ ━ ┓ ┏ ━ ━ ┓"
  , ". ┃ ┃ ┃ . ┃ ┗ ┓ . ┃"
  , ". ┃ ┃ ┃ . ┃ . ┃ ┏ ┛"
  , ". ┗ ┛ ┃ . ┣ ━ ┛ ┃ ."
  , "┏ ┓ . ┃ . ┃ ┏ ━ ┛ ."
  , "┃ ┗ ━ ┻ ━ ┛ ┃ . ┏ ┓"
  , "┗ ┳ ━ ┳ ━ ━ ╋ ━ ┛ ┃"
  , ". ┃ . ┃ ┏ ━ ┻ ━ ━ ┛"
  , ". ┃ ┏ ┛ ┃ . . . . ."
  , ". ┗ ┛ . ┗ ━ ━ ━ ━ ╾"
  ]

level1 :: Level
level1 = stringsToLevel reprOfLevel1


-- | Symbol representation for level2
reprOfLevel2 :: [String]
reprOfLevel2 =
  [ "╼ ━ ┓ . . ┏ ━ ━ ━ ┓"
  , ". . ┗ ━ ┳ ┛ . . . ┃"
  , ". . ┏ ━ ┛ . . . ┏ ┛"
  , ". ┏ ┛ ┏ ┓ ┏ ┓ . ┃ ."
  , ". ┗ ┳ ┫ ┗ ┫ ┗ ┳ ┛ ."
  , "┏ ━ ┛ ┃ . ┗ ┓ ┃ . ."
  , "┃ . ┏ ┻ ━ ┓ ┗ ╋ ━ ┓"
  , "┗ ━ ┫ . . ┗ ━ ┛ . ┃"
  , ". . ┃ . . ┏ ┓ . ┏ ┛"
  , ". . ┗ ━ ━ ┛ ┗ ━ ┻ ╾"
  ]

level2 :: Level
level2 = stringsToLevel reprOfLevel2

-- | Symbol representation for level3
reprOfLevel3 :: [String]
reprOfLevel3 =
  [ "╼ ┳ ┳ ┓ ┏ ┓ ┏ ┳ ┳ ┓"
  , "┏ ┻ ┫ ┣ ┛ ┣ ┫ ┣ ┫ ┃"
  , "┣ ┳ ┫ ┗ ┳ ┻ ╋ ┫ ┣ ┫"
  , "┗ ┫ ┣ ┳ ┫ ┏ ┻ ┫ ┣ ┛"
  , "┏ ┛ ┣ ┻ ╋ ┫ ┏ ╋ ┻ ┓"
  , "┗ ┳ ╋ ┳ ┛ ┣ ┛ ┗ ┳ ┫"
  , "┏ ┻ ┻ ┫ ┏ ┻ ┓ ┏ ┻ ┫"
  , "┣ ┓ ┏ ┫ ┗ ┳ ╋ ┛ ┏ ┫"
  , "┣ ┻ ┫ ┣ ┳ ┫ ┣ ┓ ┣ ┛"
  , "┗ ━ ┻ ┻ ┛ ┗ ┛ ┗ ┻ ╾"
  ]

level3 :: Level
level3 = stringsToLevel reprOfLevel3


-- | Rotate a pipe by 90 degrees in clockwise direction specified number of times
rotatePipeClockwise :: Int -> Pipe -> Pipe
rotatePipeClockwise n p
  | n <= 0 = p
  | otherwise =
    case p of
      (ConnectivePipe u r d l) -> rotatePipeClockwise (n - 1) (ConnectivePipe l u r d)
      _ -> p

-- | Rotate a pipe randomly
rotatePipeRandomly :: Pipe -> IO Pipe
rotatePipeRandomly p = do
  n <- Random.randomRIO (0, 3)
  return (rotatePipeClockwise n p)

-- | Randomly rotate all the pipes in the level
randomizeLevel :: Level -> IO Level
randomizeLevel = mapM (mapM randomizeCell)
  where
    randomizeCell :: Cell -> IO Cell
    randomizeCell cell =
      case cell of
        Just pipe -> fmap Just (rotatePipeRandomly pipe)
        Nothing   -> return cell

-- | Adds indeces to each element in the list
levelWithIndeces :: [[a]] -> [[(Int, Int, a)]]
levelWithIndeces = zipWith (\ rowIndex row
  -> zipWith (\ colIndex c -> (rowIndex, colIndex, c)) [0 .. ] row) [0..]
