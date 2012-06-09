{-# OPTIONS_GHC -Wall #-}

module Vis ( vis
           , display
           , animate
           , simulate
           , VisObject(..)
           , Camera0(..)
           , SpecialKey(..)
           , BitmapFont(..)
           , Flavour(..)
           , module Graphics.Gloss.Data.Color
           ) where

import Graphics.Gloss.Data.Color
import Graphics.UI.GLUT ( SpecialKey(..), BitmapFont(..), Flavour(..) )

import Vis.Camera ( Camera0(..) )
import Vis.Interface ( display, animate, simulate )
import Vis.Vis ( vis )
import Vis.VisObject ( VisObject(..) )
