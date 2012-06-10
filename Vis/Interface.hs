{-# OPTIONS_GHC -Wall #-}

module Vis.Interface ( display
                     , animate
                     , animateIO
                     , simulate
                     , simulateIO
                     , play
                     , playIO
                     ) where

import Graphics.UI.GLUT ( Key, KeyState, Position, Modifiers )

import Vis.Vis ( vis )
import Vis.Camera ( makeCamera, Camera0(..), setCamera, cameraMotion, cameraKeyboardMouse )
import Vis.VisObject ( VisObject(..) )


-- | draw a static image
display :: Real b => VisObject b -> IO ()
display visobjects = animate (\_ -> visobjects)

-- | display an animation
animate :: Real b => (Float -> VisObject b) -> IO ()
animate userDrawFun = animateIO (return . userDrawFun)

-- | display an animation impurely
animateIO :: Real b => (Float -> IO (VisObject b)) -> IO ()
animateIO userDrawFun =
  vis ts (userState0, cameraState0) simFun drawFun setCameraFun (Just kmCallback) (Just motionCallback) Nothing
  where
    ts = 0.01
    userState0 = ()
    cameraState0 = makeCamera $ Camera0 { phi0 = 60
                                        , theta0 = 20
                                        , rho0 = 7}
    drawFun (_,time) = userDrawFun time
    simFun (state,_) = return state
    kmCallback (state, camState) k0 k1 _ _ = (state, cameraKeyboardMouse camState k0 k1)
    motionCallback (state, cameraState) pos = (state, cameraMotion cameraState pos)
    setCameraFun (_,cameraState) = setCamera cameraState


-- | run a simulation
simulate :: Real b =>
            Double -- ^ sample rate
            -> world -- ^ initial state
            -> (world -> VisObject b) -- ^ draw function
            -> (Float -> world -> world) -- ^ state propogation function (takes current time and state as inputs)
            -> IO ()
simulate ts state0 userDrawFun userSimFun =
  simulateIO ts state0 (return . userDrawFun) (\t -> return . (userSimFun t))

-- | run a simulation impurely
simulateIO :: Real b =>
              Double -- ^ sample rate    
              -> world -- ^ initial state
              -> (world -> IO (VisObject b)) -- ^ draw function
              -> (Float -> world -> IO world) -- ^ state propogation function (takes current time and state as inputs)
              -> IO ()
simulateIO ts userState0 userDrawFun userSimFun =
  vis ts (userState0, cameraState0) simFun drawFun setCameraFun (Just kmCallback) (Just motionCallback) Nothing
  where
    drawFun ((userState, _),_) = userDrawFun userState
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
        Double -- ^ sample time
        -> world -- ^ initial state
        -> (world -> (VisObject b)) -- ^ draw function
        -> (Float -> world -> world) -- ^ state propogation function (takes current time and state as inputs)
        -> (world -> IO ()) -- ^ set where camera looks
        -> Maybe (world -> Key -> KeyState -> Modifiers -> Position -> world) -- ^ keyboard/mouse press callback
        -> Maybe (world -> Position -> world) -- ^ mouse drag callback
        -> Maybe (world -> Position -> world) -- ^ mouse move callback
        -> IO ()
play ts userState0 userDrawFun userSimFun =
  vis ts userState0 simFun drawFun
  where
    drawFun (userState, _) = return $ userDrawFun userState
    simFun (userState,time) = return $ userSimFun time userState


---- | play a game impurely
playIO :: Real b =>
          Double -- ^ sample time
          -> world -- ^ initial state
          -> (world -> IO (VisObject b)) -- ^ draw function
          -> (Float -> world -> IO world) -- ^ state propogation function (takes current time and state as inputs)
          -> (world -> IO ()) -- ^ set where camera looks
          -> Maybe (world -> Key -> KeyState -> Modifiers -> Position -> world) -- ^ keyboard/mouse press callback
          -> Maybe (world -> Position -> world) -- ^ mouse drag callback
          -> Maybe (world -> Position -> world) -- ^ mouse move callback
          -> IO ()
playIO ts userState0 userDrawFun userSimFun =
  vis ts userState0 simFun drawFun
  where
    drawFun (userState, _) = userDrawFun userState
    simFun (userState,time) = userSimFun time userState
