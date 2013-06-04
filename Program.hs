{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Program where

import Data.Typeable

import Expr
import Types
import Data.Monoid


-----------------------------------------------------------------------------
-- | Program - AST type

-- | A partiallly applied location for arrays expecting indexing.
type PartialLoc e a = [Index] -> Loc e a

data Program a where
  Skip      :: Program a
  Assign    :: Expr -> [Expr] -> Expr -> Program a
  Statement :: Expr -> Program a
  (:>>)     :: Program a -> Program a -> Program a
  If        :: Expr -> Program a -> Program a -> Program a
  For       :: Expr -> Expr -> (Expr -> Program a) -> Program a
  Par       :: Expr -> Expr -> (Expr -> Program a) -> Program a
  Alloc     :: Type -> (Name -> Name -> (Memory -> Dim -> Program a) -> Program a) -> Program a
  Decl      :: Type -> (Name -> Program a) -> Program a
  BasicProc :: Program a -> Program a
  OutParam  :: Type -> (Name -> Program a) -> Program a 
  InParam   :: Type -> (Memory -> Name -> Program a) -> Program a
  deriving Typeable


instance Eq (Program a) where
  a == b = False -- Dummy

instance Monoid (Program a) where
  mempty          = Skip
  mappend Skip b  = b
  mappend a  Skip = a
  mappend a b     = a .>> b
  mconcat []      = Skip
  mconcat [p]     = p
  mconcat ps      = foldl1 (.>>) ps


-- an easy-to-access test program
testFor = for (Num 0) (Num 10) (\e -> locNest "arr" [e] e)

-----------------------------------------------------------------------------
-- "Smart" Constructors for Programs

iff :: Expr -> Program a -> Program a -> Program a
iff (Num c) p q = if c /= 0 then p else q
iff c       p q = If c p q

for :: Expr -> Expr -> (Expr -> Program a) -> Program a
for (Num a) (Num b) _ | a > b = Skip
for a       b       p         = For a b p

par :: Expr -> Expr -> (Expr -> Program a) -> Program a
par (Num a) (Num b) _ | a > b = Skip
par a       b       p         = Par a b p

(.>>) :: Program a -> Program a -> Program a
Skip .>> q    = q
p    .>> Skip = p
p    .>> q    = p :>> q
infixr 0 .>>

-----------------------------------------------------------------------------
-- Locations


-- LHS of an assignment.
type Loc a b = a -> Program b

locArray :: Name -> Index -> Loc Expr a
locArray v i = \x -> Assign (var v) [i] x

locNest :: Name -> [Index] -> Loc Expr a
locNest v is = \x -> Assign (var v) is x

nil :: Loc Expr a
nil = \_ -> Skip

loc :: Name -> Loc Expr a
loc v = \x -> Assign (var v) [] x

zeroLoc :: Name -> Loc Expr a
zeroLoc v = \x -> Assign (var v) [Num 0] x

locDeref :: Name -> Loc Expr a
locDeref v = \x -> Assign (deref (var v)) [] x


-- Memory handling

memcpy :: Expr -> Size -> Type -> Loc Expr a
memcpy dst s t = copy
  where
    sz = BinOp $ Mul s $ Call (var "sizeof") [var $ show t]

    kind (Index m _ _)    = m
    kind (UnOp (Deref e)) = kind e
    kind e                = error (show e)

    copy src | src == dst = Skip
             | otherwise = 
              case (kind dst, kind src) of
               (Host, Host) -> Statement $ Call (var "memcpy") [dst, src, sz]
               (DevGlobal, Host) -> Statement $ Call (var "clEnqueueWriteBuffer")
                                      [ var "command_queue"
                                      , dst           -- target
                                      , var "CL_TRUE" -- blocking
                                      , Num 0         -- offset
                                      , sz            -- number of bytes
                                      , src           -- source
                                      , Num 0         -- num events
                                      , var "NULL"    -- event list
                                      , var "NULL"    -- completion event
                                      ]
               (DevGlobal, DevGlobal) -> Statement $ Call (var "clEnqueueCopyBuffer")
                                      [ var "command_queue"
                                      , src           -- source
                                      , dst           -- target
                                      , Num 0         -- src offset
                                      , Num 0         -- dst offset
                                      , sz            -- number of bytes
                                      , Num 0         -- num events
                                      , var "NULL"    -- event list
                                      , var "NULL"    -- completion event
                                      ]
               (Host, DevGlobal) -> Statement $ Call (var "clEnqueueReadBuffer")
                                      [ var "command_queue"
                                      , src           -- source
                                      , var "CL_TRUE" -- blocking
                                      , Num 0         -- offset
                                      , sz            -- number of bytes
                                      , dst           -- target
                                      , Num 0         -- num events
                                      , var "NULL"    -- event list
                                      , var "NULL"    -- completion event
                                      ]


free :: Expr -> Program a
free v@(Index Host n is) = Statement $ Call (var "free") [v]
free v@(Index DevGlobal n is) = Statement $ Call (var "clReleaseMemObject") [v]

