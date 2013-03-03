module Array where

import Expr

data Pull a = Pull { pull :: Index -> a }

--data Push a = Push { push :: (Index -> Loc a a) -> Program a }

-- An array is a size and an array type (Pull or Push)
data Array p a =
  Array{ size :: Size
       , doit :: p a
       }

--instance Functor Pull where
--  fmap f (Pull p) = Pull $ \i -> f (p i)
--
--instance Functor Push where
--  fmap f (Push p) = Push $ \iloc -> p (\i -> iloc i . f)

-- primitive (named) arrays
array :: Name -> Size -> Array Pull Expr
array arr n =
  Array{ size = n
       , doit = Pull $ \i -> Index arr [i]
       }

-- Converting any array type to a Push array
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
--



-- (old things for arrays)

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



