{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{- 
 - PIRE - a Parallel Intermediate Representation for Embedded languages
 - --- Built on a Representation by Koen Lindström Claessen --- 
-}

module PIRE where

import Expr
import Flatten
import Array
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
  Alloc    :: Size -> ((Index -> Loc (Expr) a) -> Array Pull (Expr) -> Program a) -> Program a
  AllocNew :: Type -> Size -> (Array Pull (Expr)) -> (Loc (Expr) a -> Array Pull (Expr) -> Program a) -> Program a
  Alloc'   :: Flatten e => Type -> Size -> Array Pull e -> (Loc e a -> Array Pull e -> Program a) -> Program a


-- TODO does Alloc's need to be part of AST?
--f :: Size -> Loc a -> Program a

--

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

-- TODO 
--f :: Size -> Loc a -> Program a


-- LHS of an assignment.
type Loc a b = a -> Program b


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

locArray :: Name -> Index -> Loc (Expr) a
locArray v i = \x -> Assign v [i] x

--locNest :: Name -> [Index] -> Loc Expr
--locNest v is = \x -> Assign v is x

-----------------------------------------------------------------------------
-- Arrays

--data InternalRepr a = Nil | Unit a | Tuple [InternalRepr a] | Loop (Array Pull (InternalRepr a))
--type FData = InternalRepr (Expr, Type)
--
--class Flatten a where
--  toFData   :: a -> FData
--  fromFData :: FData -> a
--
--instance Flatten Expr where
--  toFData e = Unit (e, TInt)
--  fromFData (Unit (e, t)) = e
--
--instance Flatten a => Flatten (Array Pull a) where
--  toFData (Array len (Pull ixf)) = Loop $ Array len (Pull $ \i -> toFData $ ixf i)
--  fromFData (Loop arr) = Array (size arr) (Pull $ \x -> fromFData $ (pull $ doit arr) x)
-- We have two array types, Pull and Push.
-- TODO: explain difference

--data Pull a = Pull { pull :: Index -> a }
--
--data Push a = Push { push :: (Index -> Loc a a) -> Program a }
--
---- An array is a size and an array type (Pull or Push)
--data Array p a =
--  Array{ size :: Size
--       , doit :: p a
--       }
--
----instance Functor Pull where
----  fmap f (Pull p) = Pull $ \i -> f (p i)
----
----instance Functor Push where
----  fmap f (Push p) = Push $ \iloc -> p (\i -> iloc i . f)
--
---- primitive (named) arrays
--array :: Name -> Size -> Array Pull Expr
--array arr n =
--  Array{ size = n
--       , doit = Pull $ \i -> Index arr [i]
--       }
--
---- Converting any array type to a Push array
--class Pushable p where
--  toPush :: Array p a -> Array Push a
--
--instance Pushable Push where
--  toPush = id
--
--instance Pushable Pull where
--  toPush arr =
--    Array{ size = size arr
--         , doit = Push $ \iloc -> For (Num 0) (size arr) (\i -> iloc i (pull (doit arr) i))
--         } 

---- memorize
--memorize :: Array Push Expr -> (Array Pull Expr -> Program) -> Program
--memorize arr f = Alloc (size arr) $ \iloc arr' -> push (doit arr) iloc .>> f arr'

---- reverse
--class Indexed p where
--  imap :: (Index -> Index) -> p a -> p a
--
--instance Indexed Pull where
--  imap f (Pull p) = Pull $ \i -> p (f i)
--
--instance Indexed Push where
--  imap f (Push p) = Push $ \iloc -> p (\i -> iloc (f i))
--
--rev :: Indexed p => Array p a -> Array p a
--rev arr =  arr{ doit = imap (\i -> ((size arr :-: Num 1) :-: i)) (doit arr) }
--
---- map
--
--instance Functor p => Functor (Array p) where
--  fmap f arr = arr{ doit = fmap f (doit arr) }
--
---- zip/unzip
--
--zipp :: Array Pull a -> Array Pull b -> Array Pull (a,b)
--zipp arr1 arr2 =
--  Array{ size = size arr1
--       , doit = Pull $ \i -> (pull (doit arr1) i, pull (doit arr2) i)
--       }
--
---- concatenation
--
--(+>+) :: (Pushable p1, Pushable p2) => Array p1 a -> Array p2 a -> Array Push a
--arr1 +>+ arr2 =
--  Array{ size = size arr1 :+: size arr2
--       , doit = Push $ \iloc -> push (doit (toPush arr1)) iloc
--                            .>> push (doit (toPush arr2)) (\i -> iloc (size arr1 :+: i))
--       }
--
--(+=+) :: Array Pull a -> Array Pull a -> Array Push a
--arr1 +=+ arr2 = 
--  Array{ size = size arr1 :+: size arr2
--       , doit = Push $ \iloc -> push (doit zarr) (\i -> iloc i & iloc (i :+: size zarr))
--       }
-- where 
--  zarr = toPush (zipp arr1 arr2)
--
--halve :: Array Pull a -> (Array Pull a, Array Pull a)
--halve arr =
--  ( arr{ size = size arr ./ Num 2 }
--  , arr{ size = size arr ./ Num 2
--       , doit = Pull $ \i -> pull (doit arr) (i .+ (size arr ./ Num 2))
--       }
--  )
--
---- pair/unpair
--
--pair :: Array Pull a -> Array Pull (a,a)
--pair arr =
--  Array{ size = size arr ./ Num 2
--       , doit = Pull $ \i -> (pull (doit arr) (i ./ Num 2), pull (doit arr) ((i ./ Num 2) .+ Num 1))
--       }
--
--unpair :: Pushable p => Array p (a,a) -> Array Push a
--unpair arr =
--  Array{ size = size arr .* Num 2
--       , doit = Push $ \iloc -> push (doit (toPush arr)) (\i -> iloc (i .* Num 2) & iloc ((i .* Num 2) .+ Num 1))
--       }
--
---- riffle
--
--riffle :: Array Pull a -> Array Push a
--riffle = unpair . uncurry zipp . halve



