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
