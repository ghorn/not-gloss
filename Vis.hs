{-# OPTIONS_GHC -Wall #-}

module Vis ( display
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

import Vis.Interface ( display, animate, simulate, play, animateIO, simulateIO, playIO )
import Vis.VisObject ( VisObject(..) )
import Vis.GlossColor
