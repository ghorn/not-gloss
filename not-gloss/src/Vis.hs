{-# OPTIONS_GHC -Wall #-}

module Vis ( Options(..)
           , Camera0(..)
           , defaultOpts
           , display
           , animate
           , simulate
           , play
           , animateIO
           , simulateIO
           , playIO
           , visMovie
           , VisObject(..)
           , SpecialKey(..)
           , BitmapFont(..)
           , Flavour(..)
           , LoadedObjModel
           , loadObjModel
           , module Vis.GlossColor
           ) where

import Graphics.UI.GLUT ( SpecialKey(..), BitmapFont(..), Flavour(..) )

import Vis.Vis ( Options(..), visMovie )
import Vis.Camera ( Camera0(..) )
import Vis.Interface ( display, animate, simulate, play, animateIO, simulateIO, playIO )
import Vis.VisObject ( VisObject(..), LoadedObjModel, loadObjModel )
import Vis.GlossColor

-- | Some reasonable default options.
-- Consider changing the window name with something like:
--
-- > myOptions = defaultOpts {optWindowName = "my rad program"}
defaultOpts :: Options
defaultOpts =
  Options
  { optBackgroundColor = Nothing
  , optWindowSize = Nothing
  , optWindowPosition = Nothing
  , optWindowName = "not-gloss"
  , optInitialCamera = Nothing
  }
