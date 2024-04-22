{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module MDP where

import qualified Data.Vector as V

import GridWorld

  
class ActionSpace action where
  enumerate :: [action]

class MDP state action | state -> action where
  transition :: state -> action -> state
  reward :: state -> action -> Int
  -- parametrized policy
  policy :: [[Int]] -> state -> action

  
policyTrans :: (MDP s a) => [[Int]] -> s -> s
policyTrans theta state = transition state (policy theta state)

-- run the MDP n times accourding to policy 
runMDP :: (MDP s a) => Int -> [[Int]] -> s -> s
runMDP n theta state =
  (iterate (\st -> policyTrans theta st) state) !! n
  
{- ########### TYPECLASS INSTANCES ######### -}

{-
instance Parameter Int where
  initparam _ = 1 
  
instance Parameter Float where
  initparam _ = 0.1


instance (Parameter n) => Parameter (V.Vector n) where
  initparam (x:xs) = V.replicate x (initparam xs)
 -}
-- instance Parameter (V.Vector (V.Vector n)) where
  -- 2d vectors only
--  initparam (y:x:ys) = V.replicate y (V.replicate x 0.1) 


instance ActionSpace Action where
  enumerate = [North, South, West, East]


gwEnumStates :: State -> [State]
gwEnumStates (State grid _) =
  let
    numx = (length $ head grid) - 1
    numy = (length grid) - 1
    allCoords = [(y, x) | x <- [0..numx], y <- [0..numy]]
    validCoords = filter (\ (y, x) -> (get2d grid y x) == "  ")
                          allCoords
  in
    map (\ (y, x) -> State grid (Player y x)) validCoords
    

instance MDP State Action where
  transition = transition_fn
  reward st ac = reward_fn st

  policy theta (State grid player) =
    let
      moveID = (theta !! player.row) !! player.col
    in
      case moveID of
        1  -> North
        -1 -> South
        2  -> East
        -2 -> West
        _  -> error "Parameter not shaped properly!"
   




