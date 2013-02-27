module FlattenBase (Expr, Type(..), InternalRepr (..), FData, Flatten(..)) where

import Expr
import Array
import Types


data InternalRepr a = Nil | Unit a | Tuple [InternalRepr a] | Loop (Array Pull (InternalRepr a))

type FData = InternalRepr (Expr, Type)


-- | Things that can be flattened in memory (which we in PIRE take to mean things that can be allocated).
class Flatten a where
  toFData   :: a -> FData
  fromFData :: FData -> a


