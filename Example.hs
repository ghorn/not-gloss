{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.UI.GLUT( SpecialKey(..) )
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
drawFun key (State (x,_) quat) = [box,plane]
  where
    box = VisBox (0.2, 0.2, 0.2) (Xyz 0 0 x) quat col
      where
        col = case key of Nothing -> Rgb 0 1 1
                          _       -> Rgb 1 1 0
    plane = VisPlane (Xyz 0 0 1) 0 (Rgb 1 1 1) (Rgba 0.4 0.6 0.65 0.4)

main :: IO ()
main = do
  putStrLn "press arrow keys to change color"
  let camera0 = Camera0 { phi0 = 60
                        , theta0 = 20
                        , rho0 = 7}
      state0 = State (-1.4,0) (Quat 1 0 0 0)
  vis camera0 simloop drawFun state0 ts
