{-# OPTIONS_GHC -Wall #-}

module Vis ( Options(..)
           , defaultOptions
           , display
           , animate
           , simulate
           , play
           , animateIO
           , simulateIO
           , playIO
           , VisObject(..)
           , SpecialKey(..)
           , BitmapFont(..)
           , Flavour(..)
           , module Vis.GlossColor
           ) where

import Graphics.UI.GLUT ( SpecialKey(..), BitmapFont(..), Flavour(..) )

import Vis.Vis ( Options(..) )
import Vis.Interface ( display, animate, simulate, play, animateIO, simulateIO, playIO )
import Vis.VisObject ( VisObject(..) )
import Vis.GlossColor

-- | Some reasonable default options.
-- Consider changing the window name with something like:
--
-- > myOptions = defaultOpts {optWindowName = "my rad program"}
defaultOptions :: Options
defaultOptions =
  Options
  { optBackgroundColor = Nothing
  , optWindowSize = Nothing
  , optWindowPosition = Nothing
  , optWindowName = "not-gloss"
  }
