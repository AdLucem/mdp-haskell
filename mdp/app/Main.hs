module Main (main) where

import GridWorld


moves = [North, North, East, East, East, South]

             
main :: IO ()
main = do
  print $ initgw
  let gw' = foldl (\gw a -> transition_fn gw a) initgw moves 
  print $ gw'
