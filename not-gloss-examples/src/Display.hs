{-# OPTIONS_GHC -Wall #-}

module Main where

import Linear

import Vis

drawFun :: VisObject Double
drawFun = VisObjects $ [axes,box,ellipsoid,sphere] ++ (map text [-5..5]) ++ [boxText, plane] 
  where
    x = -1
    quat = normalize $ Quaternion 1 (V3 2 3 4)
    
    axes = Axes (0.5, 15)
    sphere = Trans (V3 0 x (-1)) $ Sphere 0.15 Wireframe (makeColor 0.2 0.3 0.8 1)
    ellipsoid = Trans (V3 x 0 (-1)) $ RotQuat quat $ Ellipsoid (0.2, 0.3, 0.4) Solid (makeColor 1 0.3 0.5 1)
    box = Trans (V3 0 0 x) $ RotQuat quat $ Box (0.2, 0.2, 0.2) Wireframe (makeColor 0 1 1 1)
    plane = Plane (V3 0 0 1) (makeColor 1 1 1 1) (makeColor 0.4 0.6 0.65 0.4)
    text k = Text2d "OLOLOLOLOLO" (100,500 - k*100*x) TimesRoman24 (makeColor 0 (0.5 + x'/2) (0.5 - x'/2) 1)
      where
        x' = realToFrac $ (x + 1)/0.4*k/5
    boxText = Text3d "trololololo" (V3 0 0 (x-0.2)) TimesRoman24 (makeColor 1 0 0 1)

main :: IO ()
main = do
  display (defaultOpts {optWindowName = "display test"}) drawFun
