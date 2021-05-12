{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module MyProject where

import CodeWorld
import qualified Data.Text as Text
import qualified Data.List.Split as Split
import qualified Data.List

run :: IO ()
run = activityOf (Flows (levelToFlowLevel level1) 1000.0 [(0, 0)]) handleGame drawGame
-- run = drawingOf (drawCellAt 1 1 (Just (ConnectivePipe True True True True)))
handleGame :: Event -> GameState -> GameState
handleGame event gameState =
  case gameState of
    StartMenu -> handleMenu event
    Won pic   -> handleEndScreen gameState event
    Lost pic  -> handleEndScreen gameState event
    InGame lvl -> handleLevel (InGame lvl) event
    Flows flowLvl t toCheck -> handleFlows (Flows flowLvl t toCheck) event


handleMenu :: Event -> GameState
handleMenu = _

handleEndScreen :: GameState -> Event -> GameState
handleEndScreen state _= state

handleLevel :: GameState -> Event -> GameState
handleLevel (InGame lvl) event = _

handleFlows :: GameState -> Event -> GameState
handleFlows (Flows flowLevel t toCheck) (KeyPress "Up") =
  updatedState
    where
      updatedState = afterWave

      dt = 0.1

      afterWave =
        if not isEnd then  Flows (fst wave) (t + dt) (snd wave)
        else checkIfWon

      checkIfWon =
        if not (isLeaking (concat flowLevel)) && reachedEnd (concat flowLevel)
        then Won (drawFlowScreen flowLevel (t + dt) toCheck)
        else Lost (drawFlowScreen flowLevel (t + dt) toCheck)

      wave = waveAlgorithm flowLevel toCheck []

      isEnd = flowLevel == fst wave

      isLeaking [] = False
      isLeaking (c : rest) =
        case c of
          FilledCell _ True -> True
          _ -> isLeaking rest

      reachedEnd [] = False
      reachedEnd (c : rest) =
        case c of
          FilledCell (Just DestinationPipe) _ -> True
          _ -> reachedEnd rest

handleFlows state _ = state

-- | Try update an element at a given position in a list.
updateListAt :: Int -> (a -> a) -> [a] -> [a]
updateListAt index changeTo list = leftPart ++ newA ++ rightPart
    where
        (leftPart, leftTail) = splitAt index list
        newA = if index < 0 then [] else map changeTo (take 1 leftTail)
        rightPart = if index < 0 then leftTail else drop 1 leftTail

getListElemAt :: Int -> [a] -> Maybe a
getListElemAt _ [] = Nothing
getListElemAt 0 (a : _rest) = Just a
getListElemAt index (_a : rest)
  = if index < 0 then Nothing else getListElemAt (index - 1) rest


waveAlgorithm :: FlowLevel -> [(Int, Int)] -> [(Int, Int)] -> (FlowLevel, [(Int, Int)])
waveAlgorithm flowLevel [] acc = (flowLevel, acc)
waveAlgorithm flowLevel ((rowIndex, colIndex) : rest) acc = waveAlgorithm updFlowLevel rest updAcc
  where
    updFlowLevel = updateListAt rowIndex changeRow flowLevel
    changeRow = updateListAt colIndex changeCol
    changeCol c = case c of
                    FilledCell c' leaking -> FilledCell c' leaking
                    EmptyCell c' -> FilledCell c' (isLeaking c')

    isLeaking c' = isLeakingUp c' || isLeakingRight c' || isLeakingLeft c' || isLeakingDown c'

    isLeakingUp (Just (ConnectivePipe True _ _ _)) = (rowIndex - 1, colIndex) `notElem` updAcc && not (isFilled (rowIndex - 1, colIndex)) -- && null (getIfConnected Above)
    isLeakingUp _ = False

    isLeakingRight (Just (ConnectivePipe _ True _ _)) = (rowIndex, colIndex + 1) `notElem` updAcc && not (isFilled (rowIndex, colIndex + 1)) -- && null (getIfConnected ToRight)
    isLeakingRight _ = False

    isLeakingLeft (Just (ConnectivePipe _ _ True _)) = (rowIndex, colIndex - 1) `notElem` updAcc && not (isFilled (rowIndex, colIndex - 1)) -- && null (getIfConnected ToLeft)
    isLeakingLeft _ = False

    isLeakingDown (Just (ConnectivePipe _ _ _ True)) = (rowIndex + 1, colIndex) `notElem` updAcc && not (isFilled (rowIndex + 1, colIndex)) -- && null (getIfConnected Below)
    isLeakingDown _ = False

    isFilled (y, x) = case fmap (getListElemAt x) (getListElemAt y flowLevel) of
                        Just (Just (FilledCell _ _)) -> True
                        _ -> False

    updAcc = Data.List.nub (acc ++ addNeigborsToCheck)

    addNeigborsToCheck =
      getNotFilledCell Above ++
      getNotFilledCell ToRight ++
      getNotFilledCell Below ++
      getNotFilledCell ToLeft

    getNotFilledCell :: RelativePosition -> [(Int, Int)]
    getNotFilledCell Above
      = case fmap (getListElemAt colIndex) (getListElemAt (rowIndex - 1) flowLevel) of
          Just (Just (EmptyCell (Just ConnectivePipe {}))) -> getIfConnected Above
          _ -> []
    getNotFilledCell Below
      = case fmap (getListElemAt colIndex) (getListElemAt (rowIndex + 1) flowLevel) of
          Just (Just (EmptyCell (Just ConnectivePipe {}))) -> getIfConnected Below
          _ -> []
    getNotFilledCell ToRight
      = case fmap (getListElemAt (colIndex + 1)) (getListElemAt rowIndex flowLevel) of
          Just (Just (EmptyCell (Just ConnectivePipe {}))) -> getIfConnected ToRight
          Just (Just (EmptyCell (Just DestinationPipe))) -> getIfConnected ToRight
          _ -> []

    getNotFilledCell ToLeft
      = case fmap (getListElemAt (colIndex - 1)) (getListElemAt rowIndex flowLevel) of
          Just (Just (EmptyCell (Just ConnectivePipe {}))) -> getIfConnected ToLeft
          _ -> []


    getIfConnected :: RelativePosition -> [(Int, Int)]
    getIfConnected relPos = if checkIfConnected then [calcPosition] else []
      where
        checkIfConnected = arePipesConnected pipe1 pipe2 relPos

        pipe1 = case fmap (getListElemAt colIndex) (getListElemAt rowIndex flowLevel) of
                  Just (Just (EmptyCell (Just p))) -> p
                  Just (Just (FilledCell (Just p) _)) -> p
                  _ -> ConnectivePipe False False False False

        calcPosition = case relPos of
                        Above -> (rowIndex - 1, colIndex)
                        Below -> (rowIndex + 1, colIndex)
                        ToRight -> (rowIndex, colIndex + 1)
                        ToLeft -> (rowIndex, colIndex - 1)

        pipe2 = case fmap (getListElemAt (snd calcPosition)) (getListElemAt (fst calcPosition) flowLevel) of
                Just (Just (EmptyCell (Just p))) -> p
                Just (Just (FilledCell (Just p) _)) -> p
                _ -> ConnectivePipe False False False False


-- | Check if 2 pipes are connected given their types and relative position
arePipesConnected
  :: Pipe               -- 1st Pipe
  -> Pipe               -- 2nd Pipe
  -> RelativePosition   -- Position of the 2nd Pipe Relative to the 1st Pipe
  -> Bool

arePipesConnected (ConnectivePipe True _ _ _) (ConnectivePipe _ _ _ True) Above = True
arePipesConnected (ConnectivePipe _ True _ _) (ConnectivePipe _ _ True _) ToRight = True
arePipesConnected (ConnectivePipe _ _ True _) (ConnectivePipe _ True _ _) ToLeft = True
arePipesConnected (ConnectivePipe _ _ _ True) (ConnectivePipe True _ _ _) Below = True

arePipesConnected (ConnectivePipe _ True _ _) DestinationPipe ToRight = True
arePipesConnected SourcePipe (ConnectivePipe _ _ True _) ToRight = True

arePipesConnected _ _ _ = False


data RelativePosition = Above | Below | ToLeft | ToRight



drawGame :: GameState -> Picture
drawGame gameState =
  case gameState of
    StartMenu -> drawMenu
    Won pic -> drawWonScreen
    Lost pic -> drawLostScreen
    InGame lvl -> drawLevel lvl
    Flows flowLevel t toCheck -> drawFlowScreen flowLevel t toCheck

drawMenu :: Picture
drawMenu = _

drawWonScreen :: Picture
drawWonScreen = colored blue (circle 1)

drawLostScreen :: Picture
drawLostScreen = colored red (circle 1)

drawFlowScreen :: FlowLevel -> Double -> [(Int, Int)] -> Picture
drawFlowScreen flowLevel _ _ = pictures listOfPictures
  where
    listOfPictures
      = map (\(rowIndex, colIndex, fc)
             -> case fc of
                  FilledCell c isLeaking -> colored (getColor isLeaking) (drawCellAt colIndex (-rowIndex) c)
                  EmptyCell c -> drawCellAt colIndex (-rowIndex) c)
        (concat (levelWithIndeces flowLevel))

    getColor isLeaking = if isLeaking then red else blue


-- | Adds indeces to each element in the list
levelWithIndeces :: [[a]] -> [[(Int, Int, a)]]
levelWithIndeces = zipWith (\ rowIndex row
  -> zipWith (\ colIndex c -> (rowIndex, colIndex, c)) [0 .. ] row) [0..]

-- | Draw current state of the game
drawLevel :: Level -> Picture
drawLevel level = pictures listOfPicture
  where
    listOfPicture = map(\(rowIndex, colIndex, cell)
                        -> drawCellAt colIndex (-rowIndex) cell)
                       (concat (levelWithIndeces level))

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
  = rotated pi (drawPipe (ConnectivePipe True False True False))
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
  = rotated pi (drawPipe (ConnectivePipe True True True False))
drawPipe (ConnectivePipe True True True True) -- "╋"
  = drawPipe (ConnectivePipe True False True True)
    <> drawPipe (ConnectivePipe True True False True)
drawPipe _ = blank


-- | Draw one cell at given coordinates
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j cell = translated x y
  (rectangle 1 1 <> cellPicture)
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture =
      maybe blank drawPipe cell
      -- case cell of 
      --   Nothing -> blank
      --   Just p  -> drawPipe p

data GameState
  = StartMenu              -- ^ Start menu
  | Won Picture            -- ^ Player won (connected pipes correctly)
  | Lost Picture           -- ^ Player lost (water leakage)
  | InGame Level           -- ^ Player is in game, Level is the level he plays
  | Flows FlowLevel Double [(Int, Int)] -- ^ Player opened a valve

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

-- | Empty level of size n x m
emptyLevel :: (Int, Int) -> Level
emptyLevel (n, m) = replicate n (replicate m Nothing)

-- | Convert string representation of level to level type 
stringsToLevel :: [String] -> Level
stringsToLevel = map (map strToCell . Split.splitOn " ")
  where
    strToCell :: String -> Cell
    strToCell c =
      case c of
        "╼" -> Just SourcePipe
        "╾" -> Just DestinationPipe
        "┃" -> Just (ConnectivePipe True False False True)
        "━" -> Just (ConnectivePipe False True True False)
        "┏" -> Just (ConnectivePipe False True False True)
        "┓" -> Just (ConnectivePipe False False True True)
        "┗" -> Just (ConnectivePipe True True  False False)
        "┛" -> Just (ConnectivePipe True False True False)
        "┣" -> Just (ConnectivePipe True True False True)
        "┫" -> Just (ConnectivePipe True False True True)
        "┳" -> Just (ConnectivePipe False True True True)
        "┻" -> Just (ConnectivePipe True True True False)
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
  , ". ┃ ┃ ┛ ┃ . . . . ."
  , ". ┗ ┛ . ┗ ━ ━ ━ ━ ╾"
  ]


levelToFlowLevel :: Level -> FlowLevel
levelToFlowLevel = map (map EmptyCell)

level1 :: Level
level1 = stringsToLevel reprOfLevel1