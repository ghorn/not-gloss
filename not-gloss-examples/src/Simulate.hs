{-# OPTIONS_GHC -Wall #-}

module Main where

import Linear

import Vis

ts :: Double
ts = 0.01

data State a = State (a,a) (Quaternion a) [V3 a]

simFun :: Float -> State Double -> State Double
simFun _ (State (x0,v) q0 trail0) = State (x, v + 5*ts*(-1 - x)) (normalize (q0 * dq)) trail
  where
    dq = Quaternion 1 (V3 (x*ts) (v*ts) (x*v*ts))
    x = x0 + v*ts
    trail
      | length trail0 < 75 = (V3 0 x (-1)):trail0
      | otherwise = take 75 ((V3 0 x (-1)):trail0)

drawFun :: State Double -> VisObject Double
drawFun (State (x,_) quat trail) = VisObjects $ [axes,box,ellipsoid,sphere, line] ++ (map text [-5..5]) ++ [boxText, plane] 
  where
    axes = Axes (0.5, 15)
    sphere = Trans (V3 0 x (-1)) $ Sphere 0.15 Wireframe (makeColor 0.2 0.3 0.8 1)
    ellipsoid = Trans (V3 x 0 (-1)) $
                RotQuat quat $
                Ellipsoid (0.2, 0.3, 0.4) Solid (makeColor 1 0.3 0.5 1)
    box = Trans (V3 0 0 x) $
          RotQuat quat $
          Box (0.2, 0.2, 0.2) Wireframe (makeColor 0 1 1 1)
    plane = Plane (V3 0 0 1) (makeColor 1 1 1 1) (makeColor 0.4 0.6 0.65 0.4)
    text k = Text2d "OLOLOLOLOLO" (100,500 - k*100*x) TimesRoman24 (makeColor 0 (0.5 + x'/2) (0.5 - x'/2) 1)
      where
        x' = realToFrac $ (x + 1)/0.4*k/5
    boxText = Text3d "trololololo" (V3 0 0 (x-0.2)) TimesRoman24 (makeColor 1 0 0 1)
    line = Line' $ zip trail (map (\a -> makeColor 1 0 0 a) (linspace 1 0 (length trail))) -- (makeColor 1 0 0 1)

linspace :: Fractional a => a -> a -> Int -> [a]
linspace x0 xf n = map (\k -> x0 + (xf - x0) * (fromIntegral k) / (fromIntegral n-1)) [0..(n-1)]

main :: IO ()
main = do
  let state0 = State (-1.4,0) (Quaternion 1 (V3 0 0 0)) []
  simulate (defaultOpts {optWindowName = "simulate test"}) ts state0 drawFun simFun
