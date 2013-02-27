module FlattenBase (Expr, Type(..), InternalRepr (..), FData, Flatten(..)) where

import Expr
import Array
import Types


-- TODO parameterize over Array-type.
data InternalRepr a = Unit a | Loop (Array Pull (InternalRepr a))


-- TODO Make Expr to a, or possibly Program a?
type FData = InternalRepr (Expr, Type)

-- | Things that can be flattened in memory (which we in PIRE take to mean things that can be allocated).
class Flatten a where
  toFData   :: a -> FData
  fromFData :: FData -> a


