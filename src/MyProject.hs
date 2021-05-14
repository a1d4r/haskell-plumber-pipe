{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
module MyProject where


import CodeWorld
import qualified Data.List.Split as Split
import qualified System.Random as Random
import qualified Data.List
import qualified Data.Text as Text
import qualified Data.Maybe

run :: IO ()
run = do
  randomLevel1 <- randomizeLevel level1
  randomLevel2 <- randomizeLevel level2
  randomLevel3 <- randomizeLevel level3

  activityOf (StartMenu [randomLevel1, randomLevel2, randomLevel3]) handleGame drawGame


-- handle all states of the game
handleGame :: Event -> GameState -> GameState
handleGame event gameState =
  case gameState of
    StartMenu _  -> handleMenu gameState event
    Won _ _  -> handleEndScreen gameState event
    Lost _ _ -> handleEndScreen gameState event
    InGame _ _ -> handleLevel gameState event
    Flows _ _ _ _ _-> handleFlows gameState event



--handle the main menu
handleMenu :: GameState -> Event -> GameState
handleMenu (StartMenu levels@(lvl1 : lvl2 : lvl3 : xs)) (PointerPress (x, y)) 
  
  = newState          -- (Flows (levelToFlowLevel level1) 1 1 [(0, 0)]) 
  where 
    newState 
      | y >= -4 && y<=(-2) && (abs x) <= 4 = InGame lvl3 levels 
      | y >= -1 && y<=1 && (abs x) <= 4 = InGame lvl2 levels
      | y >= 2 && y<=4 && (abs x) <= 4 = InGame lvl1 levels
      | otherwise = StartMenu (lvl1 : lvl2 : lvl3 : xs)
    
handleMenu state _ = state

handleEndScreen :: GameState -> Event -> GameState
handleEndScreen (Won _ levels) (PointerPress _) = StartMenu levels
handleEndScreen (Lost _ levels) (PointerPress _) = StartMenu levels
handleEndScreen state _ = state


-- handle InGame State of the game
handleLevel :: GameState -> Event -> GameState
handleLevel (InGame lvl levels) (PointerPress (x, y)) = newGameState
  where 
    x' = (round (-y)) + size + 1
    y' = (round (x)) + size + 1
    size = fromIntegral $ (length lvl) `div` 2
    newGameState = case (x', y') of
      (1, 1) -> Flows (levelToFlowLevel lvl) 1 1 [(0, 0)] levels
      (i, j) -> InGame (updateAt i (updateAt j (rotateCell)) lvl) levels 
    
    rotateCell cell = case cell of
      Nothing -> cell
      Just pipe -> Just (rotatePipeClockwise 1 pipe)

handleLevel state _ = state


-- updates only one element under index "ind" using function "f"
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt ind f list = if ind > 0 then newList else list
  where
    (begin, end) = splitAt (ind-1) list
    newList = begin ++ newElem ++ (drop 1 end)
    maybeElem = Data.Maybe.listToMaybe end
    newElem = case maybeElem of
      Nothing -> []
      Just oldElem -> [f oldElem]




-- | Handle waterflow after played roated the valve
handleFlows :: GameState -> Event -> GameState
handleFlows (Flows flowLevel secsPassed t toCheck levels) (TimePassing dt) = 
  if t + dt - fromIntegral secsPassed >= 0.95 then updatedState 
    else Flows flowLevel secsPassed (t + dt) toCheck levels
  where
    updatedState =
      if not isEnd then  Flows (fst wave) (floor (t + dt)) (t + dt) (snd wave) levels 
      else finalState

    -- | Check if new and old FlowLevels are equal
    isEnd = flowLevel == fst wave

    -- | Has player won or lost
    finalState =
      if not (isLeaking (concat flowLevel)) && reachedEnd (concat flowLevel)
      then Won (drawFlowScreen flowLevel) levels
      else Lost (drawFlowScreen flowLevel) levels

    -- | Executing next wave 
    wave = waveAlgorithm flowLevel toCheck []

    -- | Check if any pipe is leaking
    isLeaking [] = False
    isLeaking (c : rest) =
      case c of
        FilledCell _ True -> True
        _ -> isLeaking rest

    -- | Check if player has reached the DestinationPipe
    reachedEnd [] = False
    reachedEnd (c : rest) =
      case c of
        FilledCell (Just DestinationPipe) _ -> True
        _ -> reachedEnd rest

handleFlows state _ = state

-- | Try to update an element at a given position in a list.   
updateListAt :: Int -> (a -> a) -> [a] -> [a]
updateListAt index changeTo list = leftPart ++ newA ++ rightPart
    where
        (leftPart, leftTail) = splitAt index list
        newA = if index < 0 then [] else map changeTo (take 1 leftTail)
        rightPart = if index < 0 then leftTail else drop 1 leftTail

-- | Get element of the list by index
getListElemAt :: Int -> [a] -> Maybe a
getListElemAt _ [] = Nothing
getListElemAt 0 (a : _rest) = Just a
getListElemAt index (_a : rest)
  = if index < 0 then Nothing else getListElemAt (index - 1) rest



waveAlgorithm 
  :: FlowLevel                    -- Level
  -> [(Int, Int)]                 -- Indeces of nodes to check in the current iteration 
  -> [(Int, Int)]                 -- Accumulator of checked nodes
  -> (FlowLevel, [(Int, Int)])    -- Return updated level and the list of indeces of nodes to check on the next iteration 
waveAlgorithm flowLevel [] acc = (flowLevel, acc)
waveAlgorithm flowLevel ((rowIndex, colIndex) : rest) acc = waveAlgorithm updFlowLevel rest updAcc
  where
    -- Marking current node (pipe) as visited (filled)
    updFlowLevel = updateListAt rowIndex changeRow flowLevel
    changeRow = updateListAt colIndex changeCol
    changeCol c = case c of
                    FilledCell c' leaking -> FilledCell c' leaking
                    EmptyCell c' -> FilledCell c' (isLeaking c')

    -- Check if the current node (pipe) is leaking
    isLeaking c' = isLeakingUp c' || isLeakingRight c' || isLeakingLeft c' || isLeakingDown c'

    isLeakingUp (Just (ConnectivePipe True _ _ _)) = null (getNeighborIfConnected Above) 
    isLeakingUp _ = False

    isLeakingRight (Just (ConnectivePipe _ True _ _)) = null (getNeighborIfConnected ToRight) 
    isLeakingRight _ = False

    isLeakingLeft (Just (ConnectivePipe _ _ _ True)) = null (getNeighborIfConnected ToLeft)
    isLeakingLeft _ = False

    isLeakingDown (Just (ConnectivePipe _ _ True _)) = null (getNeighborIfConnected Below) 
    isLeakingDown _ = False

    -- Updating the list of nodes to check in the next iteration
    updAcc = Data.List.nub (acc ++ addNeigborsToCheck)

    -- Get neighbor-pipes that are not Filled yet
    addNeigborsToCheck =
      getNeighborIfEmptyAndConnected Above ++
      getNeighborIfEmptyAndConnected ToRight ++
      getNeighborIfEmptyAndConnected Below ++
      getNeighborIfEmptyAndConnected ToLeft

    -- Get neigbor cell if it's empty and connected to current one
    getNeighborIfEmptyAndConnected :: RelativePosition -> [(Int, Int)]
    getNeighborIfEmptyAndConnected Above
      = case fmap (getListElemAt colIndex) (getListElemAt (rowIndex - 1) flowLevel) of
          Just (Just (EmptyCell (Just ConnectivePipe {}))) -> getNeighborIfConnected Above
          _ -> []
    getNeighborIfEmptyAndConnected Below
      = case fmap (getListElemAt colIndex) (getListElemAt (rowIndex + 1) flowLevel) of
          Just (Just (EmptyCell (Just ConnectivePipe {}))) -> getNeighborIfConnected Below
          _ -> []
    getNeighborIfEmptyAndConnected ToRight
      = case fmap (getListElemAt (colIndex + 1)) (getListElemAt rowIndex flowLevel) of
          Just (Just (EmptyCell (Just ConnectivePipe {}))) -> getNeighborIfConnected ToRight
          Just (Just (EmptyCell (Just DestinationPipe))) -> getNeighborIfConnected ToRight
          _ -> []
    getNeighborIfEmptyAndConnected ToLeft
      = case fmap (getListElemAt (colIndex - 1)) (getListElemAt rowIndex flowLevel) of
          Just (Just (EmptyCell (Just ConnectivePipe {}))) -> getNeighborIfConnected ToLeft
          _ -> []

    -- Get neighbor cell if it's connected to current one
    getNeighborIfConnected :: RelativePosition -> [(Int, Int)]
    getNeighborIfConnected relPos = if arePipesConnected pipe1 pipe2 relPos then [calcPosition] else []
      where
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

arePipesConnected (ConnectivePipe True _ _ _) (ConnectivePipe _ _ True _) Above = True
arePipesConnected (ConnectivePipe _ True _ _) (ConnectivePipe _ _ _ True) ToRight = True
arePipesConnected (ConnectivePipe _ _ True _) (ConnectivePipe True _ _ _) Below = True
arePipesConnected (ConnectivePipe _ _ _ True) (ConnectivePipe _ True _ _) ToLeft = True

arePipesConnected SourcePipe (ConnectivePipe _ _ _ True) ToRight = True 
arePipesConnected (ConnectivePipe _ _ _ True) SourcePipe ToLeft = True

arePipesConnected (ConnectivePipe _ True _ _) DestinationPipe ToRight = True
arePipesConnected DestinationPipe (ConnectivePipe _ True _ _) ToLeft = True

arePipesConnected _ _ _ = False


data RelativePosition = Above | Below | ToLeft | ToRight


-- draw any state of the game
drawGame :: GameState -> Picture
drawGame gameState =
  case gameState of
    StartMenu  _ -> drawMenu
    Won lvl _ -> drawWonScreen lvl
    Lost lvl _ -> drawLostScreen lvl
    InGame lvl _ -> drawLevel lvl
    Flows flowLevel _ _ _ _ -> drawFlowScreen flowLevel




-- draw the main menu
drawMenu :: Picture
drawMenu =  translated 0 shiftY menu
  where 
    dy = 3
    shiftY = 3
    
    menu = positioned1 <> positioned2 <> positioned3 
    
    positioned1 = translated 0 0 level_1  
    positioned2 = translated 0 (-dy) level_2
    positioned3 = translated 0 (2 * (-dy)) level_3
    
    level_1 = (lettering "Level 1 (Easy)") <> border1
    level_2 = (lettering "Level 2 (Normal)") <> border2
    level_3 = (lettering "Level 3 (Hard)") <> border3
    
    border1 = colored (lighter 0.4 blue) $ solidRectangle 8 2
    border2 = colored (lighter 0.4 blue) $ solidRectangle 8 2
    border3 = colored (lighter 0.4 blue) $ solidRectangle 8 2
    
    
-- draw the screen when won
drawWonScreen :: Picture -> Picture
drawWonScreen lvl = translated 0 shiftY window <> border <> lvl
  where
    dy = 2
    shiftY = 1
    border = translated (-0.5) 0 $ colored (lighter 0.4 blue) $ solidRectangle 16 6
    line1 = translated 0 0 $ lettering "You won!"
    line2 = translated 0 (-dy) $ lettering "Click anywhere to continue."
    window = line1 <> line2 <> blank


-- draw the screen when lost
drawLostScreen :: Picture -> Picture
drawLostScreen lvl = translated 0 shiftY window <> border <> lvl
    where
    dy = 2
    shiftY = 1
    border = translated (-0.5) 0 $ colored (lighter 0.4 blue) $ solidRectangle 16 6
    line1 = translated 0 0 $ lettering "You lost!"
    line2 = translated 0 (-dy) $ lettering "Click anywhere to continue."
    window = line1 <> line2 <> blank


-- draw the scene of waterflow
drawFlowScreen :: FlowLevel -> Picture
drawFlowScreen flowLevel = translated (-size) (size) (pictures listOfPictures)
  where
    size = fromIntegral $ (length flowLevel)`div` 2
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
drawLevel level = translated (-size) (size) (pictures listOfPicture)
  where
    size = fromIntegral $ (length level)`div` 2
    listOfPicture = map(\(rowIndex, colIndex, cell)
                        -> drawCellAt colIndex (-rowIndex) cell)
                       (concat (levelWithIndeces level))

-- | Draw a signle pipe
drawPipe :: Pipe -> Picture
drawPipe SourcePipe = lettering (Text.pack "\x2707")
drawPipe DestinationPipe = lettering (Text.pack "\x1F6C0")
drawPipe (ConnectivePipe False True False True) -- "━"
   = solidRectangle 1 0.2
drawPipe (ConnectivePipe True False True False) -- "┃"
  = solidRectangle 0.2 1
drawPipe (ConnectivePipe True False False True)  -- "┛"
  = solidPolygon [(-0.5, -0.1), (0.1, -0.1), (0.1, 0.5), (-0.1, 0.5), (-0.1, 0.1), (-0.5, 0.1)]
drawPipe (ConnectivePipe False False True True) -- "┓"
  = rotated (pi/2) (drawPipe (ConnectivePipe True False False True))
drawPipe (ConnectivePipe False True True False) -- "┏"
  = rotated pi (drawPipe (ConnectivePipe True False False True))
drawPipe (ConnectivePipe True True False False) -- "┗"
  = rotated (-pi/2) (drawPipe (ConnectivePipe True False False True))
drawPipe (ConnectivePipe True True False True)  -- "┻"
  = solidPolygon [(-0.5, -0.1), (0.5, -0.1), (0.5, 0.1),
                  (0.1, 0.1), (0.1, 0.5), (-0.1, 0.5),
                  (-0.1, 0.1), (-0.5, 0.1)]
drawPipe (ConnectivePipe True False True True) -- "┫"
  = rotated (pi/2) (drawPipe (ConnectivePipe True True False True))
drawPipe (ConnectivePipe True True True False) -- "┣"
  = rotated (-pi/2) (drawPipe (ConnectivePipe True True False True))
drawPipe (ConnectivePipe False True True True) -- "┳"
  = rotated pi (drawPipe (ConnectivePipe True True False True))
drawPipe (ConnectivePipe True True True True) -- "╋"
  = drawPipe (ConnectivePipe True False True True)
    <> drawPipe (ConnectivePipe True True True False)
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
  = StartMenu [Level]                    -- ^ Start menu
  | Won Picture [Level]                  -- ^ Player won (connected pipes correctly)
  | Lost Picture [Level]                 -- ^ Player lost (water leakage)
  | InGame Level [Level]                 -- ^ Player is in game, Level is the level he plays
  | Flows FlowLevel Int Double [(Int, Int)] [Level] -- ^ Player opened a valve

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
        _ -> Nothing


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


-- make FlowLevel from Level
levelToFlowLevel :: Level -> FlowLevel
levelToFlowLevel = map (map EmptyCell)

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