-- Vis.hs

{-# OPTIONS_GHC -Wall #-}

module Vis ( vis
           , VisObject(..)
           , VisColor(..)
           , Camera0(..)
           ) where

import Data.IORef ( newIORef )
import System.Exit ( exitSuccess )
import Graphics.UI.GLUT
import Data.Time.Clock
import Control.Concurrent
import Control.Monad
import Graphics.Rendering.OpenGL.Raw( glBegin, glEnd, gl_QUADS, glVertex3f )

import SpatialMath
import Vis.Camera

data VisColor = Rgb GLfloat GLfloat GLfloat
              | Rgba GLfloat GLfloat GLfloat GLfloat

setColor :: VisColor -> IO ()
setColor (Rgb r g b)    = color (Color3 r g b)
setColor (Rgba r g b a) = color (Color4 r g b a)

setMaterialDiffuse :: VisColor -> GLfloat -> IO ()
setMaterialDiffuse (Rgb r g b) a = materialDiffuse Front $= Color4 r g b a
setMaterialDiffuse (Rgba r g b a) _ = materialDiffuse Front $= Color4 r g b a

data VisObject a = VisCylinder (a,a) (Xyz a) (Quat a) VisColor
                 | VisBox (a,a,a) (Xyz a) (Quat a) VisColor
                 | VisLine [Xyz a] VisColor
                 | VisArrow (a,a) (Xyz a) (Xyz a) VisColor
                 | VisAxes (a,a) (Xyz a) (Quat a)
                 | VisPlane (Xyz a) a VisColor VisColor

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


drawObjects :: [VisObject GLdouble] -> IO ()
drawObjects = mapM_ drawObject
  where
    drawObject :: VisObject GLdouble -> IO ()
    -- cylinder
    drawObject (VisCylinder (height,radius) (Xyz x y z) (Quat q0 q1 q2 q3) col) =
      preservingMatrix $ do
        setMaterialDiffuse col 1
        setColor col
        translate (Vector3 x y z :: Vector3 GLdouble)
        rotate (2 * acos q0 *180/pi :: GLdouble) (Vector3 q1 q2 q3)
        translate (Vector3 0 0 (-height/2) :: Vector3 GLdouble)
        renderObject Solid (Cylinder' radius height 10 10)

    -- box
    drawObject (VisBox (dx,dy,dz) (Xyz x y z) (Quat q0 q1 q2 q3) col) =
      preservingMatrix $ do
        setMaterialDiffuse col 0.1
        setColor col
        translate (Vector3 x y z :: Vector3 GLdouble)
        rotate (2 * acos q0 *180/pi :: GLdouble) (Vector3 q1 q2 q3)
        normalize $= Enabled
        scale dx dy dz
        renderObject Solid (Cube 1)
        normalize $= Disabled

    -- line
    drawObject (VisLine path col) =
      preservingMatrix $ do
        lighting $= Disabled
        setColor col
        renderPrimitive LineStrip $ mapM_ (\(Xyz x' y' z') -> vertex$Vertex3 x' y' z') path
        lighting $= Enabled

    -- plane
    drawObject (VisPlane (Xyz x y z) offset col1 col2) =
      preservingMatrix $ do
        let norm = 1/(sqrt $ x*x + y*y + z*z)
            x' = x*norm
            y' = y*norm
            z' = z*norm
            r  = 10
            n  = 5
            eps = 0.01
        translate (Vector3 (offset*x') (offset*y') (offset*z') :: Vector3 GLdouble)
        rotate ((acos z')*180/pi :: GLdouble) (Vector3 (-y') x' 0)
        mapM_ drawObject $ concat [[ VisLine [Xyz (-r) y0 eps, Xyz r y0 eps] col1
                                   , VisLine [Xyz x0 (-r) eps, Xyz x0 r eps] col1
                                   ] | x0 <- [-r,-r+r/n..r], y0 <- [-r,-r+r/n..r]]
        mapM_ drawObject $ concat [[ VisLine [Xyz (-r) y0 (-eps), Xyz r y0 (-eps)] col1
                                   , VisLine [Xyz x0 (-r) (-eps), Xyz x0 r (-eps)] col1
                                   ] | x0 <- [-r,-r+r/n..r], y0 <- [-r,-r+r/n..r]]
        glBegin gl_QUADS -- start drawing a polygon (4 sided)
        setColor col2
--        glColor3f    0    1    0  -- set color to green
        let r' = realToFrac r
        glVertex3f   r'    r'  0
        glVertex3f (-r')   r'  0
        glVertex3f (-r')  (-r')  0
        glVertex3f   r'   (-r')  0
        glEnd
--
    -- arrow
    drawObject (VisArrow (size, aspectRatio) (Xyz x0 y0 z0) (Xyz x y z) col) =
      preservingMatrix $ do
        let numSlices = 8
            numStacks = 15
            cylinderRadius = 0.5*size/aspectRatio
            cylinderHeight = size
            coneRadius = 2*cylinderRadius
            coneHeight = 2*coneRadius

            rotAngle = acos(z/(sqrt(x*x + y*y + z*z) + 1e-15))*180/pi :: GLdouble
            rotAxis = Vector3 (-y) x 0
        
        translate (Vector3 x0 y0 z0 :: Vector3 GLdouble)
        rotate rotAngle rotAxis
        
        setMaterialDiffuse col 1
        setColor col
        -- cylinder
        renderObject Solid (Cylinder' cylinderRadius cylinderHeight numSlices numStacks)
        -- cone
        translate (Vector3 0 0 cylinderHeight :: Vector3 GLdouble)
        renderObject Solid (Cone coneRadius coneHeight numSlices numStacks)

    drawObject (VisAxes (size, aspectRatio) (Xyz x0 y0 z0) (Quat q0 q1 q2 q3)) = preservingMatrix $ do
      translate (Vector3 x0 y0 z0 :: Vector3 GLdouble)
      rotate (2 * acos q0 *180/pi :: GLdouble) (Vector3 q1 q2 q3)
        
      let xAxis = VisArrow (size, aspectRatio) (Xyz 0 0 0) (Xyz 1 0 0) (Rgb 1 0 0)
          yAxis = VisArrow (size, aspectRatio) (Xyz 0 0 0) (Xyz 0 1 0) (Rgb 0 1 0)
          zAxis = VisArrow (size, aspectRatio) (Xyz 0 0 0) (Xyz 0 0 1) (Rgb 0 0 1)
      drawObjects [xAxis, yAxis, zAxis]

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
   return ()


reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 40 (fromIntegral w / fromIntegral h) 0.1 100
   matrixMode $= Modelview 0
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


vis :: Camera0 -> (Maybe SpecialKey -> a -> IO a) -> (Maybe SpecialKey -> a -> [VisObject GLdouble]) -> a -> Double -> IO ()
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
  displayCallback $= display stateMVar latestKey visReadyMVar camera (\x y -> drawObjects (userDrawFun x y))
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
           
