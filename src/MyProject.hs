{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
module MyProject where

import CodeWorld


run :: IO ()
run = activityOf StartMenu handleGame drawGame

handleGame :: Event -> GameState -> GameState
handleGame event gameState =
  case gameState of
    StartMenu -> handleMenu event
    Won pic -> handleEndScreen event 
    Lost pic -> handleEndScreen event
    InGame lvl -> handleLevel (InGame lvl) event
    Flows flowLvl t -> handleFlows (Flows flowLvl t) event


handleMenu :: Event -> GameState
handleMenu = _

handleEndScreen :: Event -> GameState
handleEndScreen = _

handleLevel :: Event -> GameState
handleLevel (InGame lvl) event = _

handleFlows :: Event -> GameState
handleFlows event = _

drawGame :: GameState -> Picture
drawGame gameState =  
  case gameState of
    StartMenu -> drawMenu
    Won -> drawWonScreen
    Lost -> drawLostScreen
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
drawCellAt i j cell = translated x y
  (rectangle 1 1 <> cellPicture)
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture 

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
type FlowCell = FilledCell Cell | EmptyCell Cell

-- | A level is a 2d grid of cells
type Level = [[Cell]]

-- | A level with information about filled water
type FlowLevel = [[FlowCell]]

-- | Empty level of size n x m
emptyLevel :: (Int, Int) -> Level
emptyLevel (n, m) = replicate n (replicate m Nothing)

