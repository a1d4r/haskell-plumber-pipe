{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Game where


import           CodeWorld
import           Display
import           Levels
import           Logic
import           States

run :: IO ()
run = do
  randomLevel1 <- randomizeLevel level1
  randomLevel2 <- randomizeLevel level2
  randomLevel3 <- randomizeLevel level3

  activityOf (StartMenu [randomLevel1, randomLevel2, randomLevel3]) handleGame drawGame
