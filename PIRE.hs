{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

{- 
 - PIRE - a Parallel Intermediate Representation for Embedded languages
-}

module PIRE where

import Expr
import Types


-----------------------------------------------------------------------------
-- | Program - AST type

data Program a where
  Skip     :: Program a
  Assign   :: Name -> [Expr] -> Expr -> Program a
  (:>>)    :: Program a -> Program a -> Program a
  If       :: Expr -> Program a -> Program a -> Program a
  For      :: Expr -> Expr -> (Expr -> Program a) -> Program a
  Par      :: Expr -> Expr -> (Expr -> Program a) -> Program a
  Alloc'   :: Type -> Size -> ((Index -> Loc Expr a) -> Program a) -> Program a


--  Alloc    :: Size -> ((Index -> Loc (Expr) a) -> Array Pull (Expr) -> Program a) -> Program a
--  AllocNew :: Type -> Size -> (Array Pull (Expr)) -> (Loc (Expr) a -> Array Pull (Expr) -> Program a) -> Program a



-- TODO does Alloc's need to be part of AST?
--f :: Size -> Loc a -> Program a

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

-----------------------------------------------------------------------------
-- Locations


-- LHS of an assignment.
type Loc a b = a -> Program b

locArray :: Name -> Index -> Loc Expr a
locArray v i = \x -> Assign v [i] x

--nil :: Loc a
--nil = \_ -> Skip

--loc :: Name -> Loc Expr
--loc v = \x -> Assign v [] x
--
--(&) :: Loc a -> Loc b -> Loc (a,b)
--loc1 & loc2 = \(x,y) -> loc1 x .>> loc2 y
--
--locMap :: (b -> a) -> Loc a -> Loc b
--locMap f loc = \x -> loc (f x)



--locNest :: Name -> [Index] -> Loc Expr
--locNest v is = \x -> Assign v is x


