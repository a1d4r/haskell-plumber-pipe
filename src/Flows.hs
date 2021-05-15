module Flows where

import qualified Data.List
import           Levels
import           Utils

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
                    EmptyCell c'          -> FilledCell c' (isLeaking c')

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
    getNeighborIfConnected relPos = [calcPosition | arePipesConnected pipe1 pipe2 relPos]
      where
        pipe1 = case fmap (getListElemAt colIndex) (getListElemAt rowIndex flowLevel) of
                  Just (Just (EmptyCell (Just p))) -> p
                  Just (Just (FilledCell (Just p) _)) -> p
                  _ -> ConnectivePipe False False False False

        calcPosition = case relPos of
                        Above   -> (rowIndex - 1, colIndex)
                        Below   -> (rowIndex + 1, colIndex)
                        ToRight -> (rowIndex, colIndex + 1)
                        ToLeft  -> (rowIndex, colIndex - 1)

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
