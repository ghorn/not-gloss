{-# OPTIONS_GHC -Wall #-}

module Vis.Vis ( Options(..)
               , vis
               , FullState
               ) where

import Data.Maybe ( fromMaybe )
import Data.IORef ( newIORef )
import System.Exit ( exitSuccess )
import Data.Time.Clock ( getCurrentTime, diffUTCTime, addUTCTime )
import Control.Concurrent ( MVar, readMVar, swapMVar, newMVar, takeMVar, putMVar, forkIO, threadDelay )
import Control.Monad ( unless, forever )
import Graphics.UI.GLUT ( Capability(..), ClearBuffer(..), Color4(..), ColorMaterialParameter(..)
                        , ComparisonFunction(..), Cursor(..), DisplayMode(..), Face(..)
                        , Key(..), KeyState(..), Light(..), Modifiers(..), Position(..)
                        , ShadingModel(..), Size(..)
                        , DisplayCallback, ReshapeCallback
                        , ($=)
                        )
import qualified Graphics.UI.GLUT as GLUT
import Graphics.Rendering.OpenGL.Raw

import Vis.VisObject ( VisObject(..), drawObjects, setPerspectiveMode )
import qualified Vis.GlossColor as GC

-- | user state and internal states
type FullState a = (a, Float)

data Options =
  Options
  { -- ^ optional background color
    optBackgroundColor :: Maybe GC.Color
    -- ^ optional (x,y) window size in pixels
  , optWindowSize :: Maybe (Int,Int)
    -- ^ optional (x,y) window origin in pixels
  , optWindowPosition :: Maybe (Int,Int)
    -- ^ window name
  , optWindowName :: String
  } deriving Show

myGlInit :: Options -> IO ()
myGlInit opts = do
  GLUT.initialDisplayMode $= [ DoubleBuffered, RGBAMode, WithDepthBuffer ]

  Size x y <- GLUT.get GLUT.screenSize
  putStrLn $ "screen resolution " ++ show x ++ "x" ++ show y
  let intScale d i = round $ d*(realToFrac i :: Double)
      x0 = intScale 0.3 x
      xf = intScale 0.95 x
      y0 = intScale 0.05 y
      yf = intScale 0.95 y

      (xsize, ysize) = fromMaybe (xf - x0, yf - y0) (optWindowSize opts)
      (xpos, ypos) = fromMaybe (x0,y0) (optWindowPosition opts)

  GLUT.initialWindowSize $= Size (fromIntegral xsize) (fromIntegral ysize)
  GLUT.initialWindowPosition $= Position (fromIntegral xpos) (fromIntegral ypos)
  _ <- GLUT.createWindow (optWindowName opts)

  case optBackgroundColor opts of
    Nothing  -> GLUT.clearColor $= Color4 0 0 0 0
    Just col -> GLUT.clearColor $= Color4 (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
      where
        (r,g,b,a) = GC.rgbaOfColor col
  GLUT.shadeModel $= Smooth
  GLUT.depthFunc $= Just Less
  GLUT.lighting $= Enabled
  GLUT.light (Light 0) $= Enabled
  GLUT.ambient (Light 0) $= Color4 1 1 1 1
   
  GLUT.materialDiffuse Front $= Color4 0.5 0.5 0.5 1
  GLUT.materialSpecular Front $= Color4 1 1 1 1
  GLUT.materialShininess Front $= 100
  GLUT.colorMaterial $= Just (Front, Diffuse)

  glEnable gl_BLEND
  glBlendFunc gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA

drawScene :: MVar (FullState a) -> MVar Bool -> IO () -> (FullState a -> IO ()) -> DisplayCallback
drawScene stateMVar visReadyMVar setCameraFun userDrawFun = do
   GLUT.clear [ ColorBuffer, DepthBuffer ]
   
   -- draw the scene
   GLUT.preservingMatrix $ do
     -- set the camera's position and orientation
     setCameraFun
     
     -- call user function
     state <- readMVar stateMVar
     userDrawFun state

   GLUT.flush
   GLUT.swapBuffers
   _ <- swapMVar visReadyMVar True
   GLUT.postRedisplay Nothing


reshape :: ReshapeCallback
reshape size@(Size _ _) = do
   GLUT.viewport $= (Position 0 0, size)
   setPerspectiveMode
   GLUT.loadIdentity
   GLUT.postRedisplay Nothing


vis :: Real b =>
       Options -- ^ user options
       -> Double -- ^ sample time
       -> a   -- ^ initial state
       -> (FullState a -> IO a)             -- ^ sim function
       -> (FullState a -> IO (VisObject b, Maybe Cursor)) -- ^ draw function, can give a different cursor
       -> (a -> IO ())                      -- ^ set camera function
       -> Maybe (a -> Key -> KeyState -> Modifiers -> Position -> a) -- ^ keyboard/mouse callback
       -> Maybe (a -> Position -> a)              -- ^ motion callback
       -> Maybe (a -> Position -> a)              -- ^ passive motion callback
       -> IO ()
vis opts ts x0 userSimFun userDraw userSetCamera
  userKeyMouseCallback userMotionCallback userPassiveMotionCallback = do
  -- init glut/scene
  _ <- GLUT.getArgsAndInitialize
  
  myGlInit opts
   
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
                        Just cursor'' -> GLUT.cursor $= cursor''

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
            GLUT.postRedisplay Nothing

      motionCallback' pos = case userMotionCallback of
        Nothing -> return ()
        Just cb -> do
          (state0',ts') <- takeMVar stateMVar
          putMVar stateMVar (cb state0' pos, ts')
          GLUT.postRedisplay Nothing

      passiveMotionCallback' pos = case userPassiveMotionCallback of
        Nothing -> return ()
        Just cb -> do
          (state0',ts') <- takeMVar stateMVar
          putMVar stateMVar (cb state0' pos, ts')
          GLUT.postRedisplay Nothing

  GLUT.displayCallback $= drawScene stateMVar visReadyMVar setCamera makePictures
  GLUT.reshapeCallback $= Just reshape
  GLUT.keyboardMouseCallback $= Just exitOverride
  GLUT.motionCallback $= Just motionCallback'
  GLUT.passiveMotionCallback $= Just passiveMotionCallback'

  -- start main loop
  GLUT.mainLoop

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
    lastTime <- GLUT.get lastTimeRef
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

        GLUT.postRedisplay Nothing
       
      -- need to sleep longer
      else threadDelay usRemaining
