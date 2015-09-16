{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module Vis.Vis ( Options(..)
               , Antialiasing(..)
               , vis
               , visMovie
               , FullState
               ) where

import Codec.BMP ( BMP, packRGBA32ToBMP32, writeBMP )
import Control.Concurrent ( MVar, readMVar, swapMVar, newMVar, takeMVar, putMVar, forkIO, threadDelay )
import Control.Monad ( unless, forever )
import qualified Data.ByteString.Unsafe as BS
import Data.Maybe ( fromMaybe )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.Time.Clock ( getCurrentTime, diffUTCTime, addUTCTime )
import Data.Word ( Word8 )
import Foreign.Marshal.Alloc ( free )
import Foreign.Marshal.Array ( mallocArray )
import Foreign.Ptr ( Ptr, castPtr )
import Foreign.Storable ( sizeOf )
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT ( Capability(..), ClearBuffer(..), Color4(..), ColorMaterialParameter(..)
                        , ComparisonFunction(..), Cursor(..), DisplayMode(..), Face(..)
                        , Key(..), KeyState(..), Light(..), Modifiers(..), Position(..)
                        , ShadingModel(..), Size(..)
                        , DisplayCallback, ReshapeCallback
                        , ($=)
                        )
import Graphics.Rendering.OpenGL.Raw ( GLubyte, glEnable, glBlendFunc
                                     , gl_BLEND, gl_SRC_ALPHA, gl_ONE_MINUS_SRC_ALPHA )
import Text.Printf ( printf )
import System.Exit ( exitSuccess )

import Vis.Camera ( Camera, Camera0(..), setCamera, makeCamera, cameraKeyboardMouse, cameraMotion )
import Vis.VisObject ( VisObject(..), drawObjects, setPerspectiveMode )
import qualified Vis.GlossColor as GC

-- | user state and internal states
type FullState a = (a, Float)

data Antialiasing =
  Aliased
  | Smoothed
  | Multisampled Int
  deriving (Eq, Show, Ord)

data Options =
  Options
  { optBackgroundColor :: Maybe GC.Color -- ^ optional background color
  , optWindowSize :: Maybe (Int,Int) -- ^ optional (x,y) window size in pixels
  , optWindowPosition :: Maybe (Int,Int) -- ^ optional (x,y) window origin in pixels
  , optWindowName :: String -- ^ window name
  , optInitialCamera :: Maybe Camera0 -- ^ initial camera position
  , optAntialiasing :: Antialiasing -- ^ which antialiasing strategy to use
  } deriving Show

myGlInit :: Options -> IO ()
myGlInit opts = do
  let displayMode = [ DoubleBuffered, RGBAMode, WithDepthBuffer ] ++
        case optAntialiasing opts of
          Multisampled numSamples -> [ GLUT.Multisampling
                                     , GLUT.WithSamplesPerPixel numSamples
                                     ]
          _ -> []
  GLUT.initialDisplayMode $= displayMode

  Size x y <- GLUT.get GLUT.screenSize
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

  case optAntialiasing opts of
    Aliased -> do
      GLUT.lineSmooth $= Disabled
      GLUT.pointSmooth $= Disabled
      GLUT.multisample $= Disabled
    Smoothed -> do
      GLUT.hint GLUT.LineSmooth $= GLUT.Nicest
      GLUT.hint GLUT.PointSmooth $= GLUT.Nicest
      GLUT.lineSmooth $= Enabled
      GLUT.pointSmooth $= Enabled
      GLUT.multisample $= Disabled
    Multisampled _ -> do
      GLUT.lineSmooth $= Disabled
      GLUT.pointSmooth $= Disabled
      GLUT.multisample $= Enabled

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

      setCamera' = do
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

  GLUT.displayCallback $= drawScene stateMVar visReadyMVar setCamera' makePictures
  GLUT.reshapeCallback $= Just reshape
  GLUT.keyboardMouseCallback $= Just exitOverride
  GLUT.motionCallback $= Just motionCallback'
  GLUT.passiveMotionCallback $= Just passiveMotionCallback'

  -- start main loop
  GLUT.mainLoop

simThread :: MVar (FullState a) -> MVar Bool -> (FullState a -> IO a) -> Double -> IO ()
simThread stateMVar visReadyMVar userSimFun ts = do
  let waitUntilDisplayIsReady :: IO ()
      waitUntilDisplayIsReady = do -- todo: why not just block?
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




movieSimThread :: [VisObject a] -> MVar ([VisObject a], Camera) -> MVar Bool -> Double -> IO ()
movieSimThread objects0 stateMVar visReadyMVar ts = do
  let waitUntilDisplayIsReady :: IO ()
      waitUntilDisplayIsReady = do -- todo: why not just block?
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

    if usRemaining <= 0
      -- slept for long enough, do a sim iteration
      then do
        lastTimeRef $= addUTCTime (realToFrac ts) lastTime

        let getNextState = do
              state <- readMVar stateMVar
              let next = case state of
                    (_:xs, cs) -> (xs, cs)
                    ([], cs) -> (objects0, cs)
              return next
            putState x = swapMVar stateMVar x

        nextState <- getNextState
        _ <- nextState `seq` putState nextState

        GLUT.postRedisplay Nothing

      -- need to sleep longer
      else threadDelay usRemaining


visMovie
  :: forall b
     . Real b
     => Options -- ^ user options
     -> (Int -> FilePath) -- ^ where to write the bitmaps
     -> Double -- ^ sample time
     -> [VisObject b] -- ^ movie to draw
     -> Maybe Cursor -- ^ optional cursor
     -> IO ()
visMovie opts toFilename ts objectsToDraw maybeCursor = do
  -- init glut/scene
  _ <- GLUT.getArgsAndInitialize

  myGlInit opts

  let defaultCam =
        Camera0 { phi0 = 60
                , theta0 = 20
                , rho0 = 7}
      cameraState0 = makeCamera $ fromMaybe defaultCam (optInitialCamera opts)

  -- create internal state
  areWeDrawingRef <- newIORef False
  stateMVar <- newMVar (objectsToDraw, cameraState0)
  visReadyMVar <- newMVar False

  -- start sim thread
  _ <- forkIO $ movieSimThread objectsToDraw stateMVar visReadyMVar ts

  -- setup the callbacks
  let makePictures :: VisObject b -> Camera -> IO ()
      makePictures visobj cam = do
        GLUT.clear [ ColorBuffer, DepthBuffer ]

        -- draw the scene
        GLUT.preservingMatrix $ do
          setCamera cam
          drawObjects $ (fmap realToFrac) visobj
          case maybeCursor of
           Nothing -> return ()
           Just cursor -> GLUT.cursor $= cursor

        GLUT.flush
        GLUT.swapBuffers
        _ <- swapMVar visReadyMVar True
        GLUT.postRedisplay Nothing

      screenShot :: Int -> Camera -> (VisObject b, Int) -> IO ()
      screenShot n camera (visobj, imageNumber) = do
        -- todo: are width/height reversed?
        size@(Size width height) <- GLUT.get GLUT.windowSize
        let pos = Position 0 0
        ubytePtr <- mallocArray (fromIntegral (4*width*height)) :: IO (Ptr GLubyte)
        let pixelData = GLUT.PixelData GLUT.RGBA GLUT.UnsignedByte ubytePtr
        makePictures visobj camera
        -- "glFinish" will do the job, but it may be overkill.
        -- "swapBuffers" is probably good enough.
        -- http://stackoverflow.com/questions/2143240/opengl-glflush-vs-glfinish
        -- We just need to make sure that readPixels will do the right thing
        GLUT.finish
        GLUT.readPixels pos size pixelData
        let wordPtr :: Ptr Word8
            wordPtr
              | sizeOf (0 :: GLubyte) == sizeOf (0 :: Word8) = castPtr ubytePtr
              | otherwise = error "GLubyte size /= Word8 size"

        bs <- BS.unsafePackCStringFinalizer
              wordPtr (fromIntegral (4*width*height)) (free ubytePtr)
        let bmp :: BMP
            bmp = packRGBA32ToBMP32 (fromIntegral width) (fromIntegral height) bs

        let filename = toFilename imageNumber
            percent :: Double
            percent = 100 * fromIntegral imageNumber / fromIntegral n
        printf "writing \"%s\" (%d / %d == %6.2f %%) ...\n" filename imageNumber n percent
        writeBMP filename bmp

      drawFun = do
        areWeDrawing <- readIORef areWeDrawingRef
        (state,cam) <- readMVar stateMVar
        if areWeDrawing
          then do let n = length objectsToDraw
                  state' <- takeMVar stateMVar
                  mapM_ (screenShot n cam) (zip objectsToDraw [0..])
                  putStrLn "finished writing files"
                  putStrLn "you might want to try some command like:"
                  putStrLn "\"ffmpeg -framerate 50 -i data/movie.%03d.bmp -c:v libx264 -r 30 -pix_fmt yuv420p out.mp4\""
                  putMVar stateMVar state'

                  writeIORef areWeDrawingRef False
          else do let visobj = case (state, objectsToDraw) of
                        (y:_, _) -> y -- draw head object
                        ([], y:_) -> y -- empty state so just draw first object
                        ([], []) -> VisObjects [] -- nothing available
                  makePictures visobj cam

      exitOverride k0 k1 _k2 _k3 = case (k0,k1) of
        -- ESC button exits the program
        (Char '\27', Down) -> exitSuccess
        -- space bar starts screenshots
        (Char ' ', Down) -> writeIORef areWeDrawingRef True
        _ -> do
          (state0', cs) <- takeMVar stateMVar
          putMVar stateMVar (state0', cameraKeyboardMouse cs k0 k1)
          GLUT.postRedisplay Nothing

      motionCallback' pos = do
        (state0', cs) <- takeMVar stateMVar
        putMVar stateMVar (state0', cameraMotion cs pos)
        GLUT.postRedisplay Nothing

--      passiveMotionCallback' pos = case userPassiveMotionCallback of
--        Nothing -> return ()
--        Just cb -> do
--          (state0', cs) <- takeMVar stateMVar
--          putMVar stateMVar (cb state0' pos, cs)
--          GLUT.postRedisplay Nothing

  GLUT.displayCallback $= drawFun
  GLUT.reshapeCallback $= Just reshape
  GLUT.keyboardMouseCallback $= Just exitOverride
  GLUT.motionCallback $= Just motionCallback'
--  GLUT.passiveMotionCallback $= Just passiveMotionCallback'

  -- start main loop
  GLUT.mainLoop
