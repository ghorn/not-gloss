{-# OPTIONS_GHC -Wall #-}

module Vis.Interface ( display
                     , animate
                     , animateIO
                     , simulate
                     , simulateIO
                     , play
                     , playIO
                     ) where

import Graphics.UI.GLUT ( Key, KeyState, Position, Modifiers, Cursor(..) )

import Vis.Vis ( Options, vis )
import Vis.Camera ( makeCamera, Camera0(..), setCamera, cameraMotion, cameraKeyboardMouse )
import Vis.VisObject ( VisObject(..) )

-- | draw a static image
display :: Real b =>
           Options -- ^ user options
           -> VisObject b -- ^ object to draw
           -> IO ()
display opts visobjects = animate opts (\_ -> visobjects)

---- | display an animation
animate :: Real b =>
           Options -- ^ user options
           -> (Float -> VisObject b) -- ^ draw function
           -> IO ()
animate opts userDrawFun = animateIO opts (return . userDrawFun)

-- | display an animation impurely
animateIO :: Real b =>
             Options -- ^ user options
             -> (Float -> IO (VisObject b)) -- ^ draw function
             -> IO ()
animateIO opts userDrawFun =
  vis opts ts (userState0, cameraState0) simFun drawFun setCameraFun (Just kmCallback) (Just motionCallback) Nothing
  where
    ts = 0.01
    userState0 = ()
    cameraState0 = makeCamera $ Camera0 { phi0 = 60
                                        , theta0 = 20
                                        , rho0 = 7}
    drawFun (_,time) = do
      obs <- userDrawFun time
      return (obs, Nothing)
    simFun (state,_) = return state
    kmCallback (state, camState) k0 k1 _ _ = (state, cameraKeyboardMouse camState k0 k1)
    motionCallback (state, cameraState) pos = (state, cameraMotion cameraState pos)
    setCameraFun (_,cameraState) = setCamera cameraState


-- | run a simulation
simulate :: Real b =>
            Options -- ^ user options
            -> Double -- ^ sample rate
            -> world -- ^ initial state
            -> (world -> VisObject b) -- ^ draw function
            -> (Float -> world -> world) -- ^ state propogation function (takes current time and state as inputs)
            -> IO ()
simulate opts ts state0 userDrawFun userSimFun =
  simulateIO opts ts state0 (return . userDrawFun) (\t -> return . (userSimFun t))

-- | run a simulation impurely
simulateIO :: Real b =>
              Options -- ^ user options
              -> Double -- ^ sample rate    
              -> world -- ^ initial state
              -> (world -> IO (VisObject b)) -- ^ draw function
              -> (Float -> world -> IO world) -- ^ state propogation function (takes current time and state as inputs)
              -> IO ()
simulateIO opts ts userState0 userDrawFun userSimFun =
  vis opts ts (userState0, cameraState0) simFun drawFun setCameraFun (Just kmCallback) (Just motionCallback) Nothing
  where
    drawFun ((userState, _),_) = do
      obs <- userDrawFun userState
      return (obs, Nothing)

    simFun ((userState,cameraState),time) = do
      nextUserState <- userSimFun time userState
      return (nextUserState, cameraState)
    cameraState0 = makeCamera $ Camera0 { phi0 = 60
                                        , theta0 = 20
                                        , rho0 = 7}
    kmCallback (state, camState) k0 k1 _ _ = (state, cameraKeyboardMouse camState k0 k1)
    motionCallback (state, cameraState) pos = (state, cameraMotion cameraState pos)
    setCameraFun (_,cameraState) = setCamera cameraState


---- | play a game
play :: Real b =>
        Options -- ^ user options
        -> Double -- ^ sample time
        -> world -- ^ initial state
        -> (world -> (VisObject b, Maybe Cursor)) -- ^ draw function, can give a different cursor
        -> (Float -> world -> world) -- ^ state propogation function (takes current time and state as inputs)
        -> (world -> IO ()) -- ^ set where camera looks
        -> Maybe (world -> Key -> KeyState -> Modifiers -> Position -> world) -- ^ keyboard/mouse press callback
        -> Maybe (world -> Position -> world) -- ^ mouse drag callback
        -> Maybe (world -> Position -> world) -- ^ mouse move callback
        -> IO ()
play opts ts userState0 userDrawFun userSimFun =
  vis opts ts userState0 simFun drawFun
  where
    drawFun (userState, _) = return $ userDrawFun userState
    simFun (userState,time) = return $ userSimFun time userState


---- | play a game impurely
playIO :: Real b =>
          Options -- ^ user options
          -> Double -- ^ sample time
          -> world -- ^ initial state
          -> (world -> IO (VisObject b, Maybe Cursor)) -- ^ draw function, can give a different cursor
          -> (Float -> world -> IO world) -- ^ state propogation function (takes current time and state as inputs)
          -> (world -> IO ()) -- ^ set where camera looks
          -> Maybe (world -> Key -> KeyState -> Modifiers -> Position -> world) -- ^ keyboard/mouse press callback
          -> Maybe (world -> Position -> world) -- ^ mouse drag callback
          -> Maybe (world -> Position -> world) -- ^ mouse move callback
          -> IO ()
playIO opts ts userState0 userDrawFun userSimFun =
  vis opts ts userState0 simFun drawFun
  where
    drawFun (userState, _) = userDrawFun userState
    simFun (userState,time) = userSimFun time userState
