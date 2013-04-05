{-# LANGUAGE DeriveDataTypeable #-}
module Procedure where

import Data.Typeable

import Expr
import Program
import Types
import GenOCL

data Proc a = Proc 
            { procName :: String
            , procBody :: Program a
            , inParams :: [Name]
            , outParam :: Name
            }
  deriving (Typeable, Show)


