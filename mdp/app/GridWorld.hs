{-# LANGUAGE OverloadedRecordDot #-}

module GridWorld where


import Data.List

-- | Place element at position in list
place :: [a] -> a -> Int -> [a]
place ls obj i =
  (take i ls) ++ [obj] ++ (drop (i + 1) ls)
  
-- | Place element at position in 2D matrix
place2d :: [[a]] -> a -> Int -> Int -> [[a]]
place2d mat obj x y =
  let
    row = mat !! x
    row' = place row obj y
  in
    place mat row' x


get2d :: [[a]] -> Int -> Int -> a
get2d mat y x = (mat !! y) !! x


type Grid = [[String]]

{- " " for empty space, "#" for wall, string(int) for end state -}

initgrid :: Grid
initgrid = [["  ", "  ", "  ", " 1"],
            ["  ", "##", "  ", "-1"],
            ["  ", "  ", "  ", "  "]]

initgw :: State
initgw = State initgrid (Player 2 0)

-- | Player: player row and column
data Player = Player {row :: Int, col :: Int}
  deriving (Show, Read, Eq)

-- | Gridworld is grid + player
data State =
  State {grid :: Grid,
         player :: Player}
  deriving (Read, Eq)

instance Show State where
  show (State grid player) =
    let
      grid' = place2d grid "* " player.row player.col
      showRow row = concat (intersperse "|" row) 
    in
      concat (intersperse "\n" (map showRow grid'))
      ++ "\n\n State reward:" ++
      (show (reward_fn (State grid player))) ++
      "\n--------------------------------------"
      
-- | Action space
data Action = North | South | West | East

-- | Is action valid in given state
-- actionCheck :: State -> Action -> Bool
-- actionCheck (State grid player) Up =
  
-- | transition function
transition_fn :: State -> Action -> State
transition_fn (State grid player) action =
  let
    player' = case action of
      North -> Player (player.row - 1) player.col
      South -> Player (player.row + 1) player.col
      West -> Player player.row (player.col - 1)
      East -> Player player.row (player.col + 1)
    rowValid = (player'.row >= 0) && (player'.row < (length grid))
    colValid = (player'.col >= 0) && (player'.col < (length (grid !! 0)))
    moveValid = rowValid && colValid
    newGridSquare =
      if moveValid
      then (Just ((grid !! player'.row) !! player'.col))
      else Nothing
  in
    case newGridSquare of
      Nothing -> (State grid player)
      Just sq -> case sq of
        "##" -> (State grid player)
        _    -> (State grid player')


reward_fn :: State -> Int
reward_fn (State grid player) =
  case ((grid !! player.row) !! player.col) of
    "  " -> 0
    "##" -> error "Player inside your walls"
    cell -> read cell 
    
          
    
  
