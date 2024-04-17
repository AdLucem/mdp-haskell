{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main (main) where

import qualified Data.Vector as V
import Torch.Typed hiding (Device)
import Torch

import GridWorld
import MDP

data
  MLPSpec
    (inputFeatures :: Nat)
    (outputFeatures :: Nat)
    (hiddenFeatures :: Nat)
    (dtype :: DType)
    (device :: (DeviceType, Nat))
  = MLPSpec
  deriving (Eq, Show)

{-
type Vec = V.Vector Float
type Vec2d = V.Vector (V.Vector Float)


dot :: Vec -> Vec -> Float
dot v1 v2 = sum $ V.zipWith (*) v1 v2

cross :: Vec2d -> Vec -> Vec
cross mat vec = V.map (dot vec) mat

logit :: Float -> Float
logit x = 1 / (1 + exp (negate x))


softmax :: Vec -> Vec
softmax z = V.map (\zi -> ((exp zi) / denom)) z
  where
    denom = V.sum $ V.map exp z
    
f :: Vec2d -> Vec -> Vec
f wt x = softmax $ cross wt x

x :: Vec
x = V.fromList [1, 2, 3, 4]

y :: Vec
y = V.fromList [0, 0, 1, 0]

w :: Vec2d
w = V.replicate 3 (V.replicate 4 0.1)


crossEntropy :: Vec -> Vec -> Float
crossEntropy y p = negate $ V.sum $ V.zipWith (\a b -> a * (log b)) y p

at :: Vec2d -> Int -> Int -> Float
at mat i j = (mat V.! i) V.! j


gradientUpdate :: Float -> Vec2d -> Vec -> Vec -> Vec2d
gradientUpdate lr theta x y =
  let
    p = f theta x
    deltaAt i = (p V.! i) - (y V.! i)
    delta i = V.map (\xj -> lr * (deltaAt i) * xj) x
    update theta i = V.zipWith (-) (theta V.! i) (delta i)
  in
    V.generate (V.length theta) (\c -> update theta c)
-}


-- gradientDescent 
main :: IO ()
main = do
  print "Hello world"
