{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main (main) where

import qualified Data.Vector as V
import Control.Concurrent

import GridWorld
import MountainCar
import MDP


{- North : 1 South: -1 East: 2 West: -2 -}
gw_param :: [[Int]]
gw_param = [[2,  2,  2,  2],
            [1,  1,  1,  1],
            [1, -2, -2, -2]]

initcar :: MCState
initcar = MCState matrix 0 0


printloop :: MCState -> IO ()
printloop state = do
  print state
  print (state.position, (yPos state.position))
  putStrLn $ replicate 10 '\n' 
  threadDelay 1000000
  printloop $ policyTrans sample_bestpolicy state
  
main :: IO ()
main = printloop initcar
