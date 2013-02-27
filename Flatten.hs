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
  fromFData (Loop arr) = Array (size arr) (Pull $ \x -> fromFData $ (pull $ doit arr) x)


compileFData :: FData -> Name -> Gen (String, Expr, Name)
compileFData Nil _ = return ("",(Num 0),"")
compileFData (Unit (e,t)) mem = return $ ("",e,"")
compileFData (Loop (Array len (Pull ixf))) mem = do v <- incVar
                                                    let loopVar = ([ "i", "j", "k" ] ++ [ "i" ++ show i | i <- [0..] ]) !! v
                                                    return $ 
                                                      ("int " ++ loopVar ++ " = 0;\n" 
                                                        ++ "for(int " ++ loopVar ++ " = 0 ; "
                                                        ++ loopVar ++ " < " ++ show len ++ " ; "
                                                        ++ loopVar ++ "++ ){"
                                                      , 
                                                        (fromFData $ ixf (var (loopVar)) :: Expr)
                                                      , loopVar)

