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
           , module Graphics.Gloss.Data.Color
           ) where

import Graphics.Gloss.Data.Color
import Graphics.UI.GLUT ( SpecialKey(..), BitmapFont(..), Flavour(..) )

import Vis.Interface ( display, animate, simulate, play, animateIO, simulateIO, playIO )
import Vis.VisObject ( VisObject(..) )
