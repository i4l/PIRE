{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Procedure where

import Expr
import Program
import Types

import Data.Typeable
import Data.Monoid



-- Note that a procedure has return type unit (e.g. void).

data Proc a where
  NilProc   :: Proc a
  BasicProc :: Proc a -> Proc a
  ProcBody  :: Program a -> Proc a
  -- Give just a name since we don't know whether we want to just write or just read 
  -- to it beforehand (and thus can't make it a Loc or an Expr).
  OutParam  :: Type -> (Name -> Proc a) -> Proc a 
  NewParam  :: Type -> (Name -> Proc a) -> Proc a
  deriving (Typeable)

instance Monoid (Proc a) where
  mempty      = BasicProc NilProc
  mappend     = chainP

chainP :: Proc a -> Proc a -> Proc a
chainP NilProc              b = b
chainP (BasicProc p) (BasicProc q) = BasicProc (mappend p q)
chainP (BasicProc p) b             = BasicProc (mappend p b)
chainP a (BasicProc p)             = BasicProc (mappend a p)
chainP (ProcBody p) (ProcBody q) = ProcBody (p .>> q)
chainP a@(ProcBody prg) b = mappend b a
chainP (OutParam t   k) b = OutParam t (\n1 -> mappend (k n1) b)
chainP (NewParam t   k) b = NewParam t (\n1 -> mappend (k n1) b)



emptyProc :: Proc ()
emptyProc = BasicProc (OutParam (TPointer TInt) $ \out -> NewParam (TPointer TInt) $ \p1 ->
              ProcBody $ for (Num 0) (Num 10) $ \e -> Assign out [e] (Index p1 [e]) ) 


