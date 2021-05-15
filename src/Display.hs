{-# LANGUAGE OverloadedStrings #-}
module Display where

import           CodeWorld
import qualified Data.Text as Text
import           Levels
import           States


-- | Draw any state of the game
drawGame :: GameState -> Picture
drawGame gameState =
  case gameState of
    StartMenu  _            -> drawMenu
    Won lvl _               -> drawWonScreen lvl
    Lost lvl _              -> drawLostScreen lvl
    InGame lvl _            -> drawLevel lvl
    Flows flowLevel _ _ _ _ -> drawFlowScreen flowLevel


-- | Draw the main menu
drawMenu :: Picture
drawMenu =  translated 0 shiftY menu
  where
    dy = 3
    shiftY = 3

    menu = positioned1 <> positioned2 <> positioned3

    positioned1 = translated 0 0 level_1
    positioned2 = translated 0 (-dy) level_2
    positioned3 = translated 0 (2 * (-dy)) level_3

    level_1 = lettering "Level 1 (Easy)" <> border1
    level_2 = lettering "Level 2 (Normal)" <> border2
    level_3 = lettering "Level 3 (Hard)" <> border3

    border1 = colored (lighter 0.4 blue) $ solidRectangle 8 2
    border2 = colored (lighter 0.4 blue) $ solidRectangle 8 2
    border3 = colored (lighter 0.4 blue) $ solidRectangle 8 2


-- | Draw the screen when won
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


-- | Draw the scene of waterflow
drawFlowScreen :: FlowLevel -> Picture
drawFlowScreen flowLevel = translated (-size) size (pictures listOfPictures)
  where
    size = fromIntegral $ length flowLevel`div` 2
    listOfPictures
      = map (\(rowIndex, colIndex, fc)
             -> case fc of
                  FilledCell c isLeaking -> colored (getColor isLeaking) (drawCellAt colIndex (-rowIndex) c)
                  EmptyCell c -> drawCellAt colIndex (-rowIndex) c)
        (concat (levelWithIndeces flowLevel))

    getColor isLeaking = if isLeaking then red else blue

-- | Draw current state of the game
drawLevel :: Level -> Picture
drawLevel level = translated (-size) size (pictures listOfPicture)
  where
    size = fromIntegral $ length level`div` 2
    listOfPicture = map(\(rowIndex, colIndex, cell)
                        -> drawCellAt colIndex (-rowIndex) cell)
                       (concat (levelWithIndeces level))

-- | Draw one cell at given coordinates
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j cell = translated x y
  (rectangle 1 1 <> cellPicture)
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture =
      maybe blank drawPipe cell

-- | Draw a single pipe
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
