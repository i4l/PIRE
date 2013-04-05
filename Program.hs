{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Program where

import Data.Typeable

import Expr
import Types
import Gen
import Data.Monoid


-----------------------------------------------------------------------------
-- | Program - AST type

-- | An array is simply an Expr (e.g. a name). Expects a list of Indices.
type IndexedArray = [Index] -> Expr

-- | A partiallly applied location for arrays expecting indexing.
type PartialLoc e a = [Index] -> Loc e a

data Program a where
  Print     :: Type -> Expr -> Program a -- printf of an expression
  Skip      :: Program a
  Assign    :: Name -> [Expr] -> Expr -> Program a
  Statement :: Expr -> Program a                    -- We can turn an Expr into a Program
  (:>>)     :: Program a -> Program a -> Program a
  If        :: Expr -> Program a -> Program a -> Program a
  For       :: Expr -> Expr -> (Expr -> Program a) -> Program a
  Par       :: Expr -> Expr -> (Expr -> Program a) -> Program a
  Alloc     :: Type -> Dim -> (PartialLoc Expr a -> IndexedArray -> Program a) -> Program a
  deriving Typeable

instance Eq (Program a) where
  a == b = False

instance Monoid (Program a) where
  mempty          = Skip
  mappend Skip b  = b
  mappend a  Skip = a
  mappend a b     = a :>> b
  mconcat []      = Skip
  mconcat [p]     = p
  mconcat ps      = foldl1 (.>>) ps


-- an easy-to-access test program
testFor = for (Num 0) (Num 10) (\e -> Assign "arr" [e] e)

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
locArray v i = \x -> Assign v [i] x

locNest :: Name -> [Index] -> Loc Expr a
locNest v is = \x -> Assign v is x

--nil :: Loc a
--nil = \_ -> Skip

loc :: Name -> Loc Expr a
loc v = \x -> Assign v [] x
--
--(&) :: Loc a -> Loc b -> Loc (a,b)
--loc1 & loc2 = \(x,y) -> loc1 x .>> loc2 y
--
--locMap :: (b -> a) -> Loc a -> Loc b
--locMap f loc = \x -> loc (f x)





