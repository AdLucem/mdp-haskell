{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module MDP where

import qualified Data.Vector as V

import GridWorld


class Parameter a where
  initparam :: [Int] -> a


class ActionSpace action where
  enumerate :: [action]

class MDP state action | state -> action where
 
  transition :: state -> action -> state
  reward :: state -> action -> Int
  -- parametrized policy
  policy :: (Parameter n) => n -> state -> action

  
policyTrans :: (Parameter theta, MDP s a) => theta -> s -> s
policyTrans theta state = transition state (policy theta state)


{- ########### TYPECLASS INSTANCES ######### -}

instance Parameter Int where
  initparam _ = 1 
  
instance Parameter Float where
  initparam _ = 0.1


instance (Parameter n) => Parameter (V.Vector n) where
  initparam (x:xs) = V.replicate x (initparam xs)
  
-- instance Parameter (V.Vector (V.Vector n)) where
  -- 2d vectors only
--  initparam (y:x:ys) = V.replicate y (V.replicate x 0.1) 

gwparams :: V.Vector (V.Vector Float)
gwparams = initparam [1,5] 

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
      policygrid = [[East, East, East, East],
                    [North, North, North, North],
                    [North, West, West, West]]
    in
      (policygrid !! player.row) !! player.col
   




