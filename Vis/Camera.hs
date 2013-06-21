{-# OPTIONS_GHC -Wall #-}

module Vis.Camera ( Camera0(..)
                  , Camera(..)
                  , makeCamera
                  , setCamera
                  , cameraMotion
                  , cameraKeyboardMouse
                  ) where

import Xyz
import Graphics.UI.GLUT

data Camera0 = Camera0 { phi0 :: GLdouble
                       , theta0 :: GLdouble
                       , rho0 :: GLdouble
                       }

data Camera = Camera { phi :: GLdouble
                     , theta :: GLdouble
                     , rho :: GLdouble
                     , pos :: Xyz GLdouble
                     , ballX :: GLint
                     , ballY :: GLint 
                     , leftButton :: GLint
                     , rightButton :: GLint
                     }

makeCamera :: Camera0 -> Camera
makeCamera camera0 = Camera { phi   = phi0 camera0
                            , theta = theta0 camera0
                            , rho   = rho0 camera0
                            , pos = Xyz 0 0 0
                            , ballX = (-1)
                            , ballY = (-1)
                            , leftButton = 0
                            , rightButton = 0
                            }

setCamera :: Camera -> IO ()
setCamera camera = lookAt (Vertex3 xc yc zc) (Vertex3 x0 y0 z0) (Vector3 0 0 (-1))
  where
    Xyz x0 y0 z0 = pos camera
    phi'   = phi   camera
    theta' = theta camera
    rho'   = rho   camera

    xc = x0 + rho'*cos(phi'*pi/180)*cos(theta'*pi/180)
    yc = y0 + rho'*sin(phi'*pi/180)*cos(theta'*pi/180)
    zc = z0 - rho'*sin(theta'*pi/180)

cameraMotion :: Camera -> Position -> Camera
cameraMotion (Camera phi0' theta0' rho0' (Xyz x0 y0 z0) bx by lb rb) (Position x y) =
  Camera nextPhi nextTheta rho0' nextPos nextBallX nextBallY lb rb
  where
    deltaX
      | bx == -1  = 0
      | otherwise = fromIntegral (x - bx)
    deltaY
      | by == -1  = 0
      | otherwise = fromIntegral (y - by)
    nextTheta'
      | deltaY + theta0' >  80 =  80
      | deltaY + theta0' < -80 = -80
      | otherwise              = deltaY + theta0'
    nextX = x0 + 0.003*rho0'*( -sin(phi0'*pi/180)*deltaX - cos(phi0'*pi/180)*deltaY)
    nextY = y0 + 0.003*rho0'*(  cos(phi0'*pi/180)*deltaX - sin(phi0'*pi/180)*deltaY)

    (nextPhi, nextTheta) = if lb == 1
                           then (phi0' + deltaX, nextTheta')
                           else (phi0', theta0')

    nextPos = if rb == 1
              then Xyz nextX nextY z0
              else Xyz x0 y0 z0

    nextBallX = x
    nextBallY = y

cameraKeyboardMouse :: Camera -> Key -> KeyState -> Camera
cameraKeyboardMouse camera key keyState =
  camera {rho = newRho, leftButton = lb, rightButton = rb, ballX = bx, ballY = by}
  where
    (lb, reset0) = case (key, keyState) of (MouseButton LeftButton, Down) -> (1, True)
                                           (MouseButton LeftButton, Up) -> (0, False)
                                           _ -> (leftButton camera, False)
    (rb, reset1) = case (key, keyState) of (MouseButton RightButton, Down) -> (1, True)
                                           (MouseButton RightButton, Up) -> (0, False)
                                           _ -> (rightButton camera, False)
  
    (bx,by) = if reset0 || reset1 then (-1,-1) else (ballX camera, ballY camera)
  
    newRho = case (key, keyState) of (MouseButton WheelUp, Down)   -> 0.9 * (rho camera)
                                     (MouseButton WheelDown, Down) -> 1.1 * (rho camera)
                                     (Char 'e', Down)   -> 0.9 * (rho camera)
                                     (Char 'q', Down) -> 1.1 * (rho camera)
                                     _ -> rho camera
