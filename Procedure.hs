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
  Nil       :: Proc a
  BasicProc :: Proc a -> Proc a
  ProgProc  :: Program a -> Proc a
  -- Give just a name since we don't know whether we want to just write or just read 
  -- to it beforehand (and thus can't make it a Loc or an Expr).
  OutParam  :: Type -> (Name -> Name -> Proc a) -> Proc a
  NewParam  :: Type -> (Name -> Name -> Proc a) -> Proc a


emptyProc :: Proc ()
emptyProc = BasicProc (OutParam (TPointer TInt) $ \out outc -> NewParam (TPointer TInt) $ \p1 p1c ->
              ProgProc $ for (Num 0) (var p1c) $ \e -> Assign out [e] (Index p1 [e]) ) 
              
--data Proc a = Proc 
--            { procName :: String
--            , inParams :: [(Name, Type)]
--            , outParam :: (Name, Type)
--            , procBody :: Program a -> Proc a
--            }
--  deriving (Typeable)
--
--procInit :: String -> Type -> (Program a -> Proc a) -> Proc a
--procInit name typ = Proc name [] ("a", typ)
--
--newInput :: Type -> Program a -> (Program a -> Proc a) -> Proc a
--newInput t prog k = Proc n ins' out body
--  where Proc n ins out body = k prog
--        ins' = ("b",TInt):ins
--
--


--Proc "test" [("p1", TPointer TInt)] ("out", TPointer TInt) testFor
