{-# OPTIONS_GHC -Wall #-}

module Vis.Interface ( display
                     , animate
                     , simulate
                     ) where

import Vis.Camera ( Camera0(..) )
import Vis.VisObject ( VisObject(..) )
import Vis.Vis ( vis )

-- | draw a static image
display :: Real b => [VisObject b] -> IO ()
display visobjects = animate (\_ -> visobjects)

-- | display an animation
animate :: Real b => (Float -> [VisObject b]) -> IO ()
animate userDrawFun = do
  let ts = 0.01
      state0 = ()
      drawFun _ (_,time) = userDrawFun time
      simFun _ _ = return ()
      camera0 = Camera0 { phi0 = 60
                        , theta0 = 20
                        , rho0 = 7}

  vis camera0 simFun drawFun state0 ts

-- | run a simulation
simulate :: Real b => Double -> world -> (world -> [VisObject b]) -> (Float -> world -> world) -> IO ()
simulate ts state0 userDrawFun userSimFun = do
  let drawFun _ (state,_) = userDrawFun state
      simFun _ (state,time) = return $ userSimFun time state
      camera0 = Camera0 { phi0 = 60
                        , theta0 = 20
                        , rho0 = 7}

  vis camera0 simFun drawFun state0 ts

