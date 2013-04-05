{-# LANGUAGE DeriveDataTypeable #-}

module Procedure where

import Expr
import Program
import Types

import Data.Typeable
import Data.Monoid

-- Note that a procedure has return type unit (e.g. void).
data Proc a = Proc 
            { procName :: String
            , procBody :: Program a
            , inParams :: [(Name, Type)]
            , outParam :: (Name, Type)
            }
  deriving (Typeable)

instance Monoid (Proc a) where
  mempty      = Proc mempty mempty mempty (mempty, TInt)
  mappend a b = Proc { procName = procName b
                     , procBody = mappend (procBody a) (procBody b) 
                     , inParams = mappend (inParams a) (inParams b)
                     , outParam = outParam b
                     }
emptyProc :: Proc ()
emptyProc = Proc "test" testFor [("p1", TPointer TInt)] ("out", TInt)
