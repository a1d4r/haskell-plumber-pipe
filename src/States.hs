module States where

import           CodeWorld
import           Levels

data GameState
  = StartMenu [Level]                               -- ^ Start menu
  | Won Picture [Level]                             -- ^ Player won (connected pipes correctly)
  | Lost Picture [Level]                            -- ^ Player lost (water leakage)
  | InGame Level [Level]                            -- ^ Player is in game, Level is the level he plays
  | Flows FlowLevel Int Double [(Int, Int)] [Level] -- ^ Player opened a valve
