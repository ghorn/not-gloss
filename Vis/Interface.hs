{-# OPTIONS_GHC -Wall #-}

module Vis.Interface ( display
                     ) where

import Vis.Camera ( Camera0(..) )
import Vis.VisObject ( VisObject(..) )
import Vis.Vis ( vis )

-- | open a window and draw a static image
display :: Real b => [VisObject b] -> IO ()
display visobjects = do
  let -- set some easy defaults, then call vis
      ts = 0.01
      state0 = ()
      userSimFun _ _ = return ()
      userDrawFun _ _ = visobjects
      camera0 = Camera0 { phi0 = 60
                        , theta0 = 20
                        , rho0 = 7}

  vis camera0 userSimFun userDrawFun state0 ts
