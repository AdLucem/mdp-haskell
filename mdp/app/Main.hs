{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main (main) where

import qualified Data.Vector as V

import GridWorld
import MDP


moves = [North, North, East, East, East, South]

             
main :: IO ()
main = do
  print $ initgw
  -- let gw' = transition initgw (  
  print $ V.fromList [1..10]
