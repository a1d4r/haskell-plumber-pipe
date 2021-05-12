{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
module MyProject where

import CodeWorld
import Data.List.Split
import System.Random


run :: IO ()
run = do
  g <- newStdGen 
  print ""
  -- activityOf StartMenu handleGame drawGame

handleGame :: Event -> GameState -> GameState
handleGame event gameState =
  case gameState of
    StartMenu -> handleMenu event
    Won pic -> handleEndScreen event
    Lost pic -> handleEndScreen event
    InGame lvl -> handleLevel event (InGame lvl)
    Flows flowLvl t -> handleFlows event (Flows flowLvl t)


handleMenu :: Event -> GameState
handleMenu = _

handleEndScreen :: Event -> GameState
handleEndScreen = _

handleLevel :: Event -> GameState -> GameState
handleLevel = _

handleFlows :: Event -> GameState -> GameState
handleFlows event = _

drawGame :: GameState -> Picture
drawGame gameState =
  case gameState of
    StartMenu -> drawMenu
    Won pic -> drawWonScreen
    Lost pic -> drawLostScreen
    InGame lvl -> drawLevel lvl
    Flows flowLevel t -> drawFlowScreen flowLevel t

drawMenu :: Picture
drawMenu = _

drawWonScreen :: Picture
drawWonScreen = _

drawLostScreen :: Picture
drawLostScreen = _

drawLevel :: Level -> Picture
drawLevel lvl = _

drawFlowScreen :: FlowLevel -> Double -> Picture
drawFlowScreen flowLvl t = _

-- | Draw one cell at given coordinates
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j cell = _

data GameState
  = StartMenu              -- ^ Start menu
  | Won Picture            -- ^ Player won (connected pipes correctly)
  | Lost Picture           -- ^ Player lost (water leakage)
  | InGame Level           -- ^ Player is in game, Level is the level he plays
  | Flows FlowLevel Double -- ^ Player opened a valve

-- | Pipe that connects sides specified by True
-- | sides are specified in order Up, Right, Left, Down
data Pipe
  = ConnectivePipe   -- ^ Pipes connecting several sides, open sides are marked as True
  {
    up :: Bool,
    right :: Bool,
    down :: Bool,
    left :: Bool
  }
  | SourcePipe
  | DestinationPipe
  deriving (Show)

-- | A cell is either empty or has pipe in it
-- | Some Cells are meant to be empty by designers of the level
type Cell = Maybe Pipe

-- | A cell which can be either empty or filled with water 
data FlowCell = FilledCell Cell | EmptyCell Cell

-- | A level is a 2d grid of cells
type Level = [[Cell]]

-- | A level with information about filled water
type FlowLevel = [[FlowCell]]

-- | Empty level of size n x m
emptyLevel :: (Int, Int) -> Level
emptyLevel (n, m) = replicate n (replicate m Nothing)


-- | Convert string representation of level to level type 
stringsToLevel :: [String] -> Level
stringsToLevel = map (map strToCell . splitOn " ")
  where
    strToCell :: String -> Cell
    strToCell c =
      case c of
        "╼" -> Just SourcePipe
        "╾" -> Just DestinationPipe
        "┃" -> Just (ConnectivePipe True False True False)
        "━" -> Just (ConnectivePipe False True True False)
        "┏" -> Just (ConnectivePipe False True True False)
        "┓" -> Just (ConnectivePipe False False True True)
        "┗" -> Just (ConnectivePipe True True False False)
        "┛" -> Just (ConnectivePipe True False False True)
        "┣" -> Just (ConnectivePipe True True True False)
        "┫" -> Just (ConnectivePipe True False True True)
        "┳" -> Just (ConnectivePipe False True True True)
        "┻" -> Just (ConnectivePipe True True False True)
        "╋" -> Just (ConnectivePipe True True True True)
        _ -> Nothing


-- | Symbol representation for level
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
  n <- randomRIO (0, 3)
  return (rotatePipeClockwise n p)

