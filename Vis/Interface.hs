{-# OPTIONS_GHC -Wall #-}

module Vis.Interface ( vis
                     ) where

import Data.IORef ( newIORef )
import System.Exit ( exitSuccess )
import Data.Time.Clock ( getCurrentTime, diffUTCTime, addUTCTime )
import Control.Concurrent ( MVar, readMVar, swapMVar, newMVar, forkIO, threadDelay )
import Control.Monad ( when, unless, forever )
import Graphics.UI.GLUT

import Vis.Camera ( Camera(..) , makeCamera, Camera0(..) )
import Vis.VisObject ( VisObject(..), drawObjects, setPerspectiveMode )

myGlInit :: String -> IO ()
myGlInit progName = do
  initialDisplayMode $= [ DoubleBuffered, RGBMode, WithDepthBuffer ]
  Size x y <- get screenSize
  putStrLn $ "screen resolution " ++ show x ++ "x" ++ show y
  let intScale d i = round $ d*(realToFrac i :: Double)
      x0 = intScale 0.3 x
      xf = intScale 0.95 x
      y0 = intScale 0.05 y
      yf = intScale 0.95 y
  initialWindowSize $= Size (xf - x0) (yf - y0)
  initialWindowPosition $= Position (fromIntegral x0) (fromIntegral y0)
  _ <- createWindow progName

  clearColor $= Color4 0 0 0 0
  shadeModel $= Smooth
  depthFunc $= Just Less
  lighting $= Enabled
  light (Light 0) $= Enabled
  ambient (Light 0) $= Color4 1 1 1 1
   
  materialDiffuse Front $= Color4 0.5 0.5 0.5 1
  materialSpecular Front $= Color4 1 1 1 1
  materialShininess Front $= 25
  colorMaterial $= Just (Front, Diffuse)


display :: MVar a -> MVar (Maybe SpecialKey) -> MVar Bool -> Camera -> (Maybe SpecialKey -> a -> IO ()) -> DisplayCallback
display stateMVar keyRef visReadyMVar camera userDrawFun = do
   clear [ ColorBuffer, DepthBuffer ]
   
   -- draw the scene
   preservingMatrix $ do
     -- setup the camera
     x0     <- get (x0c    camera)
     y0     <- get (y0c    camera)
     z0     <- get (z0c    camera)
     phi'   <- get (phi   camera)
     theta' <- get (theta camera)
     rho'   <- get (rho   camera)
     let
       xc = x0 + rho'*cos(phi'*pi/180)*cos(theta'*pi/180)
       yc = y0 + rho'*sin(phi'*pi/180)*cos(theta'*pi/180)
       zc = z0 - rho'*sin(theta'*pi/180)
     lookAt (Vertex3 xc yc zc) (Vertex3 x0 y0 z0) (Vector3 0 0 (-1))
     
     -- call user function
     state <- readMVar stateMVar
     latestKey <- readMVar keyRef
     userDrawFun latestKey state

     ---- draw the torus
     --color (Color3 0 1 1 :: Color3 GLfloat)
     --renderObject Solid (Torus 0.275 1.85 8 15)
   
   flush
   swapBuffers
   _ <- swapMVar visReadyMVar True
   postRedisplay Nothing
   return ()


reshape :: ReshapeCallback
reshape size@(Size _ _) = do
   viewport $= (Position 0 0, size)
   setPerspectiveMode
   loadIdentity
   postRedisplay Nothing

keyboardMouse :: Camera -> MVar (Maybe SpecialKey) -> KeyboardMouseCallback
keyboardMouse camera keyRef key keyState _ _ =
  case (key, keyState) of
    -- kill sim thread when main loop finishes
    (Char '\27', Down) -> exitSuccess

    -- set keyRef
    (SpecialKey k, Down)   -> do
      _ <- swapMVar keyRef (Just k)
      return ()
    (SpecialKey _, Up)   -> do
      _ <- swapMVar keyRef Nothing
      return ()

    -- adjust camera
    (MouseButton LeftButton, Down) -> do 
      resetMotion
      leftButton camera $= 1
    (MouseButton LeftButton, Up) -> leftButton camera $= 0
    (MouseButton RightButton, Down) -> do 
      resetMotion
      rightButton camera $= 1
    (MouseButton RightButton, Up) -> rightButton camera $= 0
      
    (MouseButton WheelUp, Down) -> zoom 0.9
    (MouseButton WheelDown, Down) -> zoom 1.1
    
    _ -> return ()
    where resetMotion = do
            ballX camera $= -1
            ballY camera $= -1

          zoom factor = do
            rho camera $~ (* factor)
            postRedisplay Nothing
            

motion :: Camera -> MotionCallback
motion camera (Position x y) = do
   x0  <- get (x0c camera)
   y0  <- get (y0c camera)
   bx  <- get (ballX camera)
   by  <- get (ballY camera)
   phi' <- get (phi camera)
   theta' <- get (theta camera)
   rho' <- get (rho camera)
   lb <- get (leftButton camera)
   rb <- get (rightButton camera)
   let deltaX
         | bx == -1  = 0
         | otherwise = fromIntegral (x - bx)
       deltaY
         | by == -1  = 0
         | otherwise = fromIntegral (y - by)
       nextTheta 
         | deltaY + theta' >  80 =  80
         | deltaY + theta' < -80 = -80
         | otherwise             = deltaY + theta'
       nextX0 = x0 + 0.003*rho'*( -sin(phi'*pi/180)*deltaX - cos(phi'*pi/180)*deltaY)
       nextY0 = y0 + 0.003*rho'*(  cos(phi'*pi/180)*deltaX - sin(phi'*pi/180)*deltaY)
       
   when (lb == 1) $ do
     phi   camera $~ (+ deltaX)
     theta camera $= nextTheta
   
   when (rb == 1) $ do
     x0c camera $= nextX0
     y0c camera $= nextY0
   
   ballX camera $= x
   ballY camera $= y
   
   postRedisplay Nothing


vis :: Real b => Camera0 -> (Maybe SpecialKey -> a -> IO a) -> (Maybe SpecialKey -> a -> [VisObject b]) -> a -> Double -> IO ()
vis camera0 userSimFun userDrawFun x0 ts = do
  -- init glut/scene
  (progName, _args) <- getArgsAndInitialize
  myGlInit progName
   
  -- create internal state
  stateMVar <- newMVar x0
  camera <- makeCamera camera0
  visReadyMVar <- newMVar False
  latestKey <- newMVar Nothing

  -- start sim thread
  _ <- forkIO $ simThread stateMVar visReadyMVar userSimFun ts latestKey
  
  -- setup callbacks
  displayCallback $= display stateMVar latestKey visReadyMVar camera (\x y -> drawObjects $ map (fmap realToFrac) (userDrawFun x y))
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse camera  latestKey)
  motionCallback $= Just (motion camera)

  -- start main loop
  mainLoop


simThread :: MVar a -> MVar Bool -> (Maybe SpecialKey -> a -> IO a) -> Double -> MVar (Maybe SpecialKey) -> IO ()
simThread stateMVar visReadyMVar userSimFun ts keyRef = do
  let waitUntilDisplayIsReady :: IO ()
      waitUntilDisplayIsReady = do 
        visReady <- readMVar visReadyMVar
        unless visReady $ do
          threadDelay 10000
          waitUntilDisplayIsReady
  
  waitUntilDisplayIsReady
  
  t0 <- getCurrentTime
  lastTimeRef <- newIORef t0

  forever $ do
    -- calculate how much longer to sleep before taking a timestep
    currentTime <- getCurrentTime
    lastTime <- get lastTimeRef
    let usRemaining :: Int
        usRemaining = round $ 1e6*(ts - realToFrac (diffUTCTime currentTime lastTime))

    if usRemaining <= 0
      -- slept for long enough, do a sim iteration
      then do
        lastTimeRef $= addUTCTime (realToFrac ts) lastTime

        let getNextState = do
              state <- readMVar stateMVar
              latestKey <- readMVar keyRef
              userSimFun latestKey state

        let putState = swapMVar stateMVar

        nextState <- getNextState
        _ <- nextState `seq` putState nextState

        postRedisplay Nothing
       
      -- need to sleep longer
      else threadDelay usRemaining
           
