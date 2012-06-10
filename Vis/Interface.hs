{-# OPTIONS_GHC -Wall #-}

module Vis.Interface ( display
                     , animate
                     , animateIO
                     , simulate
                     , simulateIO
                     ) where

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
  vis ts (userState0, cameraState0) simFun drawFun setCameraFun kmCallback motionCallback
  where
    ts = 0.01
    userState0 = ()
    cameraState0 = makeCamera $ Camera0 { phi0 = 60
                                        , theta0 = 20
                                        , rho0 = 7}
    drawFun (_,time) = userDrawFun time
    simFun (state,_) = return state
    kmCallback (state, camState) k0 k1 = (state, cameraKeyboardMouse camState k0 k1)
    motionCallback (state, cameraState) pos = (state, cameraMotion cameraState pos)
    setCameraFun (_,cameraState) = setCamera cameraState


-- | run a simulation
simulate :: Real b => Double -> world -> (world -> VisObject b) -> (Float -> world -> world) -> IO ()
simulate ts state0 userDrawFun userSimFun =
  simulateIO ts state0 (return . userDrawFun) (\t -> return . (userSimFun t))

-- | run a simulation impurely
simulateIO :: Real b => Double -> world -> (world -> IO (VisObject b)) -> (Float -> world -> IO world) -> IO ()
simulateIO ts userState0 userDrawFun userSimFun =
  vis ts (userState0, cameraState0) simFun drawFun setCameraFun kmCallback motionCallback
  where
    drawFun ((userState, _),_) = userDrawFun userState
    simFun ((userState,cameraState),time) = do
      nextUserState <- userSimFun time userState
      return (nextUserState, cameraState)
    cameraState0 = makeCamera $ Camera0 { phi0 = 60
                                        , theta0 = 20
                                        , rho0 = 7}
    kmCallback (state, camState) k0 k1 = (state, cameraKeyboardMouse camState k0 k1)
    motionCallback (state, cameraState) pos = (state, cameraMotion cameraState pos)
    setCameraFun (_,cameraState) = setCamera cameraState
