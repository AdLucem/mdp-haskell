{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module MDP where

import qualified Data.Vector as V

import GridWorld

data Params 
class ActionSpace action where
  enumerate :: [action]

class MDP state action | state -> action where
  transition :: state -> action -> state
  reward :: state -> action -> Int
  -- parametrized policy
  policy :: (Num n) => V.Vector n -> state -> action

  
policyTrans :: (Num n, MDP s a) => V.Vector n -> s -> s
policyTrans v state = transition state (policy v state)


{- ########### TYPECLASS INSTANCES ######### -}

instance ActionSpace Action where
  enumerate = [North, South, West, East]

  
instance MDP State Action where
  transition = transition_fn
  reward st ac = reward_fn st

  policy _ (State grid player) =
    let
      policygrid = [[East, East, East, East],
                    [North, North, North, North],
                    [North, West, West, West]]
    in
      (policygrid !! player.row) !! player.col
     




