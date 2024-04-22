{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module MountainCar where

import MDP


data MCAction = MCAction Int
              deriving (Show, Read, Eq)


data MCState = MCState {env      :: [[String]],
                        position :: Float,
                        velocity :: Float}
               deriving (Read, Eq)


instance Show MCState where
  show state =
    let
      xaxis = map (\x -> x/10) $ take 19 $ iterate (\x -> x + 1) (-12)
      yaxis = map yPos xaxis
      xaxis_ = [x + 1.2 | x <- xaxis] 
      hillCoords = zip xaxis_ yaxis
      matrixHill = drawHill state.env hillCoords
      matrixWithCar = placeCar state matrixHill
    
      joinWith :: String -> [String] -> String
      joinWith d ls = foldl (\s el -> s ++ d ++ el) "" ls
    in
      joinWith "\n" $ reverse $ map (joinWith " ") matrixWithCar 

  
roundTo :: Int -> Float -> Float
roundTo n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)

ycrd :: [(Float, Float)]
ycrd = [(-1.2,0.7),(-1.1,0.6),(-1.0,0.5),(-0.9,0.4),(-0.8,0.3),(-0.7,0.2),(-0.6,0.1),(-0.5,0.2),(-0.4,0.3),(-0.3,0.4),(-0.2,0.5),(-0.1,0.6),(0.0,0.7), (0.1,0.8),(0.2,0.9),(0.3,1.0),(0.4,1.1),(0.5,1.2),(0.6,1.3)]

-- infer y position based on x position
yPos :: Float -> Float
yPos x
  | (x == -0.6) = 0.0
  | (x < -1.2) = 0.7
  | (x > 0.6) = 1.3
  | otherwise = roundTo 1 $ abs (x + 0.6)


-- replace element with 'e' at given position in list
replaceAt :: [a] -> a -> Int -> [a]
replaceAt ls e i =
  (take i ls) ++ [e] ++ (drop (i + 1) ls)

  
-- replace element at given (x, y) in matrix
replaceAt2d :: String -> [[String]] -> (Float, Float) -> [[String]]
replaceAt2d e mat (x_, y_) =
  let
    x = floor (x_ * 10)
    y = floor (y_ * 10)
    newRow = replaceAt (mat !! y) e x
  in
    replaceAt mat newRow y
    
  
-- sequentially apply a list of hillcoords to the matrix
drawHill :: [[String]] -> [(Float, Float)] -> [[String]]
drawHill mat [] = mat
drawHill mat (crds:rest) =
  drawHill (replaceAt2d "-" mat crds) rest


-- given x position, find x and y position in matrix 
carCoords :: Float -> (Float, Float)
carCoords x
  | (x < -1.2) = (-1.2, (yPos 1.2))
  | (x > 0.6)  = (0.6, (yPos 0.6))
  | otherwise  =
      let
        x' = (roundTo 1 x) + 0.6
      in
        (x', (yPos (x' - 1.3)))


placeCar :: MCState -> [[String]] -> [[String]]
placeCar (MCState _ pos _) mat =
  let
    (x, y) = carCoords pos
  in
    replaceAt2d "*" mat (x, y)  
        

newVelocity :: MCState -> Int -> Float
newVelocity (MCState _ pos vel) ac_ =
  let
    ac = fromIntegral ac_
  in
    vel + ((ac - 1.0) * 0.1) - (cos (3.0 * pos) * 0.025)

-- given existing velocity, find new position
newPosition :: MCState -> Float
newPosition (MCState _ pos vel) =
  let
    naivepos = pos + vel
  in
    if (naivepos < -1.2)
    then -1.2
    else if naivepos > 0.6
    then 0.6
    else naivepos
     

newState :: MCState -> MCAction -> MCState
newState state (MCAction action) =
  let
    newvel = newVelocity state action
    newpos = newPosition (MCState state.env state.position newvel)
  in
    MCState state.env newpos newvel


matrix :: [[String]]
matrix = [[" " | i <- [0..18]] | i <- [0..18]]

showState :: MCState -> String
showState state =
  let
    xaxis = map (\x -> x/10) $ take 19 $ iterate (\x -> x + 1) (-12)
    yaxis = map yPos xaxis
    xaxis_ = [x + 1.2 | x <- xaxis] 
    hillCoords = zip xaxis_ yaxis
    matrixHill = drawHill state.env hillCoords
    matrixWithCar = placeCar state matrixHill
    
    joinWith :: String -> [String] -> String
    joinWith d ls = foldl (\s el -> s ++ d ++ el) "" ls
  in
    joinWith "\n" $ reverse $ map (joinWith " ") matrixWithCar 


sample_bestpolicy :: [[Int]]
sample_bestpolicy = [[1, 1, 1, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]

instance MDP MCState MCAction where
  transition state action = newState state action
  reward state action = 0
  policy theta state =
    MCAction ((theta !! 0) !! (floor ((state.position + 1.2) * 10))) 


