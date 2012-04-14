{-# OPTIONS_GHC -Wall #-}

module Vis.Xyz ( Xyz(..)
               ) where

data Xyz a = Xyz a a a deriving (Show, Eq)

instance (Num a) => Num (Xyz a) where
  (Xyz x0 y0 z0) + (Xyz x1 y1 z1) = Xyz (x0 + x1) (y0 + y1) (z0 + z1)
  (Xyz x0 y0 z0) - (Xyz x1 y1 z1) = Xyz (x0 - x1) (y0 - y1) (z0 - z1)
  negate (Xyz x y z) = Xyz (-x) (-y) (-z)
  (*) = error "(*) undefined for Xyz"
  abs = error "abs undefined for Xyz"
  signum = error "signum undefined for Xyz"
  fromInteger = error "fromInteger undefined for Xyz"

instance Functor Xyz where
  fmap f (Xyz x y z) = Xyz (f x) (f y) (f z)
