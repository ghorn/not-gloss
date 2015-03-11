{-# OPTIONS_GHC -Wall #-}

module Vis ( Options(..)
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
           , module Vis.GlossColor
           ) where

import Graphics.UI.GLUT ( SpecialKey(..), BitmapFont(..), Flavour(..) )

import Vis.Vis ( Options(..), visMovie )
import Vis.Interface ( display, animate, simulate, play, animateIO, simulateIO, playIO )
import Vis.VisObject ( VisObject(..) )
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
  }
