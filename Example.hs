{-# OPTIONS_GHC -Wall #-}

module Main where

import SpatialMath
import qualified Quat

import Vis

ts :: Double
ts = 0.01

data State a = State (a,a) (Quat a)

simloop :: Maybe SpecialKey -> State Double -> IO (State Double)
simloop _ (State (x,v) q0) = return $ State (x + v*ts, v + 5*ts*(-1 - x)) (Quat.qmult' q0 dq)
  where
    dq = Quat 1 (x*ts) (v*ts) (x*v*ts)

drawFun :: Maybe SpecialKey -> State Double -> [VisObject Double]
drawFun key (State (x,_) quat) = [axes,box,plane] ++ (map text [-5..5]) ++ [boxText]
  where
    axes = VisAxes (0.5, 15) (Xyz 0 0 0) (Quat 1 0 0 0)
    box = VisBox (0.2, 0.2, 0.2) (Xyz 0 0 x) quat col
      where
        col = case key of Nothing -> makeColor 0 1 1 1
                          _       -> makeColor 1 1 0 1
    plane = VisPlane (Xyz 0 0 1) 0 (makeColor 1 1 1 1) (makeColor 0.4 0.6 0.65 0.4)
    text k = Vis2dText "OLOLOLOLOLO" (100,500 - k*100*x) TimesRoman24 (makeColor 0 (0.5 + x'/2) (0.5 - x'/2) 1)
      where
        x' = realToFrac $ (x + 1)/0.4*k/5
    boxText = Vis3dText "trololololo" (Xyz 0 0 (x-0.2)) TimesRoman24 (makeColor 1 0 0 1)

main :: IO ()
main = do
  putStrLn "press arrow keys to change color"
  let camera0 = Camera0 { phi0 = 60
                        , theta0 = 20
                        , rho0 = 7}
      state0 = State (-1.4,0) (Quat 1 0 0 0)
  vis camera0 simloop drawFun state0 ts
