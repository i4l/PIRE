{-# LANGUAGE FlexibleInstances #-}

-- | Defines instances for the class Flatten

module Flatten (Flatten (..)) where

import FlattenBase
import Array

instance Flatten Expr where
  toFData e = Unit (e, TInt)
  fromFData (Unit (e, t)) = e

instance Flatten a => Flatten (Array Pull a) where
  toFData (Array len (Pull ixf)) = Loop $ Array len (Pull $ \i -> toFData $ ixf i)
  fromFData (Loop arr) = Array (size arr) (Pull $ \x -> fromFData $ (pull $ doit arr) x)


