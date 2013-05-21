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
  Alloc     :: Type -> (Name -> Name -> (Dim -> Program a) -> Program a) -> Program a
  Decl      :: Type -> (Name -> Program a) -> Program a
  BasicProc :: Program a -> Program a
  OutParam  :: Type -> (Name -> Program a) -> Program a 
  InParam   :: Type -> (Name -> Program a) -> Program a
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

--nil :: Loc a
--nil = \_ -> Skip

loc :: Name -> Loc Expr a
loc v = \x -> Assign (var v) [] x

zeroLoc :: Name -> Loc Expr a
zeroLoc v = \x -> Assign (var v) [Num 0] x

locDeref :: Name -> Loc Expr a
locDeref v = \x -> Assign (deref (var v)) [] x


-- Memory handling

memcpy :: Expr -> Size -> Type -> Loc Expr a
memcpy v1 s t = \x -> Statement $ Call (var "memcpy") [v1, x, s']
  where s' = BinOp $ Mul s $ Call (var "sizeof") [var $ show t]

free :: Expr -> Program a
free v = Statement $ Call (var "free") [v]
