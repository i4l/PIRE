{-# LANGUAGE DeriveDataTypeable #-}

module Procedure where

import Expr
import Program
import Types
import GenOCL

import Data.Typeable
import Data.Monoid

data Proc a = Proc 
            { procName :: String
            , procBody :: Program a
            , inParams :: [Name]
            , outParam :: Name
            }
  deriving (Typeable, Show)

instance Monoid (Proc a) where
  mempty      = Proc mempty mempty mempty mempty
  mappend a b = Proc { procName = procName b
                     , procBody = mappend (procBody a) (procBody b) 
                     , inParams = mappend (inParams a) (inParams b)
                     , outParam = outParam b
                     }

