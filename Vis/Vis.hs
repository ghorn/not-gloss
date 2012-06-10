{-# OPTIONS_GHC -Wall #-}

module Vis.Vis ( vis
               , FullState
               ) where

import Data.IORef ( newIORef )
import System.Exit ( exitSuccess )
import Data.Time.Clock ( getCurrentTime, diffUTCTime, addUTCTime )
import Control.Concurrent ( MVar, readMVar, swapMVar, newMVar, takeMVar, putMVar, forkIO, threadDelay )
import Control.Monad ( unless, forever )
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.Raw

import Vis.VisObject ( VisObject(..), drawObjects, setPerspectiveMode )

-- user state and internal states
type FullState a = (a, Float)

myGlInit :: String -> IO ()
myGlInit progName = do
  initialDisplayMode $= [ DoubleBuffered, RGBAMode, WithDepthBuffer ]
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
  materialShininess Front $= 100
  colorMaterial $= Just (Front, Diffuse)

  glEnable gl_BLEND
  glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA

drawScene :: MVar (FullState a) -> MVar Bool -> IO () -> (FullState a -> IO ()) -> DisplayCallback
drawScene stateMVar visReadyMVar setCameraFun userDrawFun = do
   clear [ ColorBuffer, DepthBuffer ]
   
   -- draw the scene
   preservingMatrix $ do
     -- set the camera's position and orientation
     setCameraFun
     
     -- call user function
     state <- readMVar stateMVar
     userDrawFun state

   flush
   swapBuffers
   _ <- swapMVar visReadyMVar True
   postRedisplay Nothing


reshape :: ReshapeCallback
reshape size@(Size _ _) = do
   viewport $= (Position 0 0, size)
   setPerspectiveMode
   loadIdentity
   postRedisplay Nothing


vis :: Real b =>
       Double -- ^ sample time
       -> a   -- ^ initial state
       -> (FullState a -> IO a)             -- ^ sim function
       -> (FullState a -> IO (VisObject b, Maybe Cursor)) -- ^ draw function, can give a different cursor
       -> (a -> IO ())                      -- ^ set camera function
       -> Maybe (a -> Key -> KeyState -> Modifiers -> Position -> a) -- ^ keyboard/mouse callback
       -> Maybe (a -> Position -> a)              -- ^ motion callback
       -> Maybe (a -> Position -> a)              -- ^ passive motion callback
       -> IO ()
vis ts x0 userSimFun userDraw userSetCamera
  userKeyMouseCallback userMotionCallback userPassiveMotionCallback = do
  -- init glut/scene
  (progName, _) <- getArgsAndInitialize
  
  myGlInit progName
   
  -- create internal state
  let fullState0 = (x0, 0)
  stateMVar <- newMVar fullState0
  visReadyMVar <- newMVar False

  -- start sim thread
  _ <- forkIO $ simThread stateMVar visReadyMVar userSimFun ts
  
  -- setup the callbacks
  let makePictures x = do
        (visobs,cursor') <- userDraw x
        drawObjects $ (fmap realToFrac) visobs
        case cursor' of Nothing -> return ()
                        Just cursor'' -> cursor $= cursor''

      setCamera = do
        (state,_) <- readMVar stateMVar
        userSetCamera state

      -- kill sim thread when someone hits ESC
      exitOverride k0 k1 k2 k3 = case (k0,k1) of
        (Char '\27', Down) -> exitSuccess
        _ -> case userKeyMouseCallback of
          Nothing -> return ()
          Just cb -> do
            (state0',time) <- takeMVar stateMVar
            putMVar stateMVar (cb state0' k0 k1 k2 k3, time)
            postRedisplay Nothing

      motionCallback' pos = case userMotionCallback of
        Nothing -> return ()
        Just cb -> do
          (state0',ts') <- takeMVar stateMVar
          putMVar stateMVar (cb state0' pos, ts')
          postRedisplay Nothing

      passiveMotionCallback' pos = case userPassiveMotionCallback of
        Nothing -> return ()
        Just cb -> do
          (state0',ts') <- takeMVar stateMVar
          putMVar stateMVar (cb state0' pos, ts')
          postRedisplay Nothing

  displayCallback $= drawScene stateMVar visReadyMVar setCamera makePictures
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just exitOverride
  motionCallback $= Just motionCallback'
  passiveMotionCallback $= Just passiveMotionCallback'

  -- start main loop
  mainLoop

simThread :: MVar (FullState a) -> MVar Bool -> (FullState a -> IO a) -> Double -> IO ()
simThread stateMVar visReadyMVar userSimFun ts = do
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
        secondsSinceStart = realToFrac (diffUTCTime currentTime t0)
    
    if usRemaining <= 0
      -- slept for long enough, do a sim iteration
      then do
        lastTimeRef $= addUTCTime (realToFrac ts) lastTime

        let getNextState = do
              state <- readMVar stateMVar
              userSimFun state
            putState x = swapMVar stateMVar (x, secondsSinceStart)

        nextState <- getNextState
        _ <- nextState `seq` putState nextState

        postRedisplay Nothing
       
      -- need to sleep longer
      else threadDelay usRemaining
