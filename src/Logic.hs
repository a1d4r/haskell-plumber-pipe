module Logic where

import           CodeWorld
import           Display
import           Flows
import           Levels
import           States
import           Utils


-- | Handle all states of the game
handleGame :: Event -> GameState -> GameState
handleGame event gameState =
  case gameState of
    StartMenu _ -> handleMenu gameState event
    Won _ _     -> handleEndScreen gameState event
    Lost _ _    -> handleEndScreen gameState event
    InGame _ _  -> handleLevel gameState event
    Flows {}    -> handleFlows gameState event

-- | Handle the main menu
handleMenu :: GameState -> Event -> GameState
handleMenu (StartMenu levels@(lvl1 : lvl2 : lvl3 : xs)) (PointerPress (x, y))
  = newState          -- (Flows (levelToFlowLevel level1) 1 1 [(0, 0)])
  where
    newState
      | y >= -4 && y<=(-2) && abs x <= 4 = InGame lvl3 levels
      | y >= -1 && y<=1 && abs x <= 4 = InGame lvl2 levels
      | y >= 2 && y<=4 && abs x <= 4 = InGame lvl1 levels
      | otherwise = StartMenu (lvl1 : lvl2 : lvl3 : xs)

handleMenu state _ = state

handleEndScreen :: GameState -> Event -> GameState
handleEndScreen (Won _ levels) (PointerPress _)  = StartMenu levels
handleEndScreen (Lost _ levels) (PointerPress _) = StartMenu levels
handleEndScreen state _                          = state


-- | Handle InGame State of the game
handleLevel :: GameState -> Event -> GameState
handleLevel (InGame lvl levels) (PointerPress (x, y)) = newGameState
  where
    x' = round (-y) + size + 1
    y' = round x + size + 1
    size = fromIntegral $ length lvl `div` 2
    newGameState = case (x', y') of
      (1, 1) -> Flows (levelToFlowLevel lvl) 1 1 [(0, 0)] levels
      (i, j) -> InGame (updateAt i (updateAt j rotateCell) lvl) levels

    rotateCell cell = case cell of
      Nothing   -> cell
      Just pipe -> Just (rotatePipeClockwise 1 pipe)

handleLevel state _ = state

-- | Handle waterflow after player rotated the valve
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
        _                 -> isLeaking rest

    -- | Check if player has reached the DestinationPipe
    reachedEnd [] = False
    reachedEnd (c : rest) =
      case c of
        FilledCell (Just DestinationPipe) _ -> True
        _                                   -> reachedEnd rest

handleFlows state _ = state
