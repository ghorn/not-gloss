{-# OPTIONS_GHC -Wall #-}

module Vis.Quat ( Quat(..)
                ) where

data Quat a = Quat a a a a deriving (Show, Eq)

instance Functor Quat where
  fmap f (Quat q0 q1 q2 q3) = Quat (f q0) (f q1) (f q2) (f q3)
