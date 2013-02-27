{-# LANGUAGE FlexibleInstances #-}

-- | Defines instances for the class Flatten

module Flatten (Flatten (..), compileFData) where

import FlattenBase
import Array
import Expr

import Gen

-- TODO Other types?
instance Flatten Expr where
  toFData e = Unit (e, TInt)
  fromFData (Unit (e, t)) = e

-- TODO Parameterize over array-type (Push and Pull).
instance Flatten a => Flatten (Array Pull a) where
  toFData (Array len (Pull ixf)) = Loop $ Array len (Pull $ \i -> toFData $ ixf i)
  fromFData (Loop arr)           = Array (size arr) (Pull $ \x -> fromFData $ (pull $ doit arr) x)


compileFData :: FData -> Gen (String, Expr, Name)
compileFData (Unit (e,t))                  = return $ ("",e,"")
compileFData (Loop (Array len (Pull ixf))) = do 
  v <- incVar
  let loopVar = ([ "i", "j", "k" ] ++ [ "i" ++ show i | i <- [0..] ]) !! v
  return 
    (    "int " ++ loopVar ++ ";\n" 
      ++ "for(" ++ loopVar ++ " = 0 ; "
      ++ loopVar ++ " < " ++ show len ++ " ; "
      ++ loopVar ++ "++ ){"
    , 
      fromFData $ ixf (var loopVar) :: Expr
    , loopVar)

