{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
module MyProject where

import CodeWorld
import Data.Text as Text


run :: IO ()
-- run = activityOf StartMenu handleGame drawGame
run = drawingOf (drawCellAt 1 1 (Just (ConnectivePipe True True True True)))
handleGame :: Event -> GameState -> GameState
handleGame event gameState =
  case gameState of
    StartMenu -> handleMenu event
    Won pic   -> handleEndScreen event 
    Lost pic  -> handleEndScreen event
    InGame lvl -> handleLevel (InGame lvl) event
    Flows flowLvl t -> handleFlows (Flows flowLvl t) event


handleMenu :: Event -> GameState
handleMenu = _

handleEndScreen :: Event -> GameState
handleEndScreen = _

handleLevel :: GameState -> Event -> GameState
handleLevel (InGame lvl) event = _

handleFlows :: GameState -> Event -> GameState
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

-- | Draw a signle pipe
drawPipe :: Pipe -> Picture
drawPipe SourcePipe = lettering (Text.pack "\x2707")
drawPipe DestinationPipe = lettering (Text.pack "\x1F6C0")
drawPipe (ConnectivePipe False True True False) -- "━"
   = solidRectangle 1 0.2  
drawPipe (ConnectivePipe True False False True) -- "┃"
  = solidRectangle 0.2 1  
drawPipe (ConnectivePipe True False True False)  -- "┛"
  = solidPolygon [(-0.5, -0.1), (0.1, -0.1), (0.1, 0.5), (-0.1, 0.5), (-0.1, 0.1), (-0.5, 0.1)]
drawPipe (ConnectivePipe False False True True) -- "┓"
  = rotated (pi/2) (drawPipe (ConnectivePipe True False True False))
drawPipe (ConnectivePipe False True False True) -- "┏"
  = rotated (pi) (drawPipe (ConnectivePipe True False True False))
drawPipe (ConnectivePipe True True False False) -- "┗"
  = rotated (-pi/2) (drawPipe (ConnectivePipe True False True False))
drawPipe (ConnectivePipe True True True False) -- "┻"
  = solidPolygon [(-0.5, -0.1), (0.5, -0.1), (0.5, 0.1), 
                  (0.1, 0.1), (0.1, 0.5), (-0.1, 0.5), 
                  (-0.1, 0.1), (-0.5, 0.1)]
drawPipe (ConnectivePipe True False True True) -- "┫"
  = rotated (pi/2) (drawPipe (ConnectivePipe True True True False))
drawPipe (ConnectivePipe True True False True) -- "┣"
  = rotated (-pi/2) (drawPipe (ConnectivePipe True True True False))
drawPipe (ConnectivePipe False True True True) -- "┳"
  = rotated (pi) (drawPipe (ConnectivePipe True True True False))
drawPipe (ConnectivePipe True True True True) -- "╋"
  = drawPipe (ConnectivePipe True False True True)  
    <> drawPipe (ConnectivePipe True True False True)


-- | Draw one cell at given coordinates
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j cell = translated x y
  (rectangle 1 1 <> cellPicture)
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture =
      case cell of 
        Nothing -> blank
        Just p  -> drawPipe p

data GameState 
  = StartMenu              -- ^ Start menu
  | Won Picture            -- ^ Player won (connected pipes correctly)
  | Lost Picture           -- ^ Player lost (water leakage)
  | InGame Level           -- ^ Player is in game, Level is the level he plays
  | Flows FlowLevel Double -- ^ Player opened a valve

-- | Pipe that connects sides specified by True
-- | sides are specified in order Up, Right, Down, Left
data Pipe 
  = ConnectivePipe   -- ^ Pipes connecting several sides
  {                  -- ^ open sides are marked as True
    up :: Bool,
    right :: Bool, 
    left :: Bool, 
    down :: Bool 
  }
  | SourcePipe
  | DestinationPipe

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
emptyLevel (n, m) = Prelude.replicate n (Prelude.replicate m Nothing)

