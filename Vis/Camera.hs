{-# OPTIONS_GHC -Wall #-}

module Vis.Camera ( Camera0(..)
                  , Camera(..)
                  , makeCamera
                  ) where

import Data.IORef ( IORef, newIORef )
import Graphics.UI.GLUT ( GLdouble, GLint )

data Camera0 = Camera0 { phi0 :: GLdouble
                       , theta0 :: GLdouble
                       , rho0 :: GLdouble
                       }

data Camera = Camera { phi :: IORef GLdouble
                     , theta :: IORef GLdouble
                     , rho :: IORef GLdouble
                     , x0c :: IORef GLdouble
                     , y0c :: IORef GLdouble
                     , z0c :: IORef GLdouble
                     , ballX :: IORef GLint
                     , ballY :: IORef GLint 
                     , leftButton :: IORef GLint
                     , rightButton :: IORef GLint
                     }

makeCamera :: Camera0 -> IO Camera
makeCamera camera0 = do
  phi'   <- newIORef $ phi0 camera0
  theta' <- newIORef $ theta0 camera0
  rho'   <- newIORef $ rho0 camera0
  x0    <- newIORef 0
  y0    <- newIORef 0
  z0    <- newIORef 0
  ballX'  <- newIORef (-1)
  ballY'  <- newIORef (-1)
  leftButton' <- newIORef 0
  rightButton' <- newIORef 0
  return $ Camera { phi = phi',
                    theta = theta',
                    rho = rho',
                    x0c = x0,
                    y0c = y0,
                    z0c = z0,
                    ballX = ballX',
                    ballY = ballY',
                    leftButton = leftButton',
                    rightButton = rightButton'
                  }

