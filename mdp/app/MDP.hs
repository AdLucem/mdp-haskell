{-# LANGUAGE MultiParamTypeClasses #-}

module MDP where

import GridWorld

class MDP state action where
  transition :: state -> action -> state
  reward :: state -> Int
  policy :: state -> action


instance MDP Gridworld Action where
  transition = transition_fn
  reward = reward_fn

  policy (State grid player) =
    let
      policygrid = [[East, East, East, East],
                    [North, North, North, North],
                    [North, West, West, West]]
    in
      (policygrid !! player.row) !! player.col
     




