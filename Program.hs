{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}


module Program where

import Expr
import Types


-----------------------------------------------------------------------------
-- | Program - AST type

-- | An array is simply an Expr (e.g. a name). Expects a list of Indices.
type IndexedArray = [Index] -> Expr

-- | A partiallly applied location for arrays expecting indexing.
type PartialLoc e a = [Index] -> Loc e a

data Program a where
  Skip     :: Program a
  Assign   :: Name -> [Expr] -> Expr -> Program a
  (:>>)    :: Program a -> Program a -> Program a
  If       :: Expr -> Program a -> Program a -> Program a
  For      :: Expr -> Expr -> (Expr -> Program a) -> Program a
  Par      :: Expr -> Expr -> (Expr -> Program a) -> Program a

  -- We need the 'IndexedArray' to access the initialized memory
  Alloc   :: Type -> Dim -> (PartialLoc Expr a -> IndexedArray -> Program a) -> Program a



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

--loc :: Name -> Loc Expr
--loc v = \x -> Assign v [] x
--
--(&) :: Loc a -> Loc b -> Loc (a,b)
--loc1 & loc2 = \(x,y) -> loc1 x .>> loc2 y
--
--locMap :: (b -> a) -> Loc a -> Loc b
--locMap f loc = \x -> loc (f x)




