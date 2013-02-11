{-
 - PIRE - a Parallel Intermediate Representation for Embedded languages
 - --- Built on a Representation by Koen Lindström Claessen --- 
-}

module PIRE where


-----------------------------------------------------------------------------
-- Expressions

type Name = String

data Expr
  = Num Int
  | Index Name [Expr]
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :/: Expr
  | Expr :*: Expr
  | Expr :<=: Expr
 deriving ( Eq )

type Size  = Expr
type Index = Expr

var :: Name -> Expr
var v = Index v []

-- This instance is quite limited.
instance Ord Expr where
  e1 <= e2 = (toInt e1) <= (toInt e2)
    where
      toInt :: Expr -> Int
      toInt (Num n)    = n
      toInt (a :-: b)  = (toInt a) - (toInt b)
      toInt (a :/: b)  = (toInt a) `div` (toInt b)
      toInt (a :*: b)  = (toInt a) * (toInt b)


instance Show Expr where
  show (Num n)      = show n
  show (Index a is) = a ++ concat [ "[" ++ show i ++ "]" | i <- is ]
  show (a :+: b)    = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (a :-: b)    = "(" ++ show a ++ "-" ++ show b ++ ")"
  show (a :/: b)    = "(" ++ show a ++ "/" ++ show b ++ ")"
  show (a :*: b)    = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (a :<=: b)   = "(" ++ show a ++ " <= " ++ show b ++ ")"


-----------------------------------------------------------------------------
-- "Smart" Constructors for expressions

(.+), (.-), (.<=), (./), (.*) :: Expr -> Expr -> Expr
Num 0 .+ b     = b
a     .+ Num 0 = a
Num a .+ Num b = Num (a+b)
a     .+ b     = a :+: b

Num a .- Num b = Num (a-b)
a     .- Num 0 = a
a     .- b     = a :-: b

Num a ./ Num b = Num (a `div` b)
Num 0 ./ b     = Num 0
a     ./ Num 1 = a
a     ./ b     = a :/: b

Num a .* Num b = Num (a*b)
a     .* Num 0 = Num 0
a     .* Num 1 = a
Num 0 .* b     = Num 0
Num 1 .* b     = b
a     .* b     = a :*: b

Num a .<= Num b      = Num (if a<=b then 1 else 0)
a     .<= b | a == b = Num 1
a     .<= b          = a :<=: b

-----------------------------------------------------------------------------
-- Program - AST type

data Program
  = Skip
  | Assign Name [Expr] Expr
  | Program :>> Program
  | If Expr Program Program
  | For Expr Expr (Expr -> Program)
  | Par Expr Expr (Expr -> Program) -- Parallel Loop

-- Splitting these into two allows us to potentially reuse old decls/names (I think)
  | Alloc Size ((Index -> Loc Expr) -> Array Pull Expr -> Program)

  -- | AllocNew Type Size (Loc Expr -> Array Pull Expr -> Program)
  | AllocNew Type Size (Array Pull Expr) (Loc Expr -> Array Pull Expr -> Program)
data Type = TInt | TChar | TFloat | TPointer Type

instance Show Type where
  show TInt = "int"
  show TChar = "char"
  show TFloat = "float"
  show (TPointer t) = show t ++ "*"

-----------------------------------------------------------------------------
-- "Smart" Constructors for Programs

iff :: Expr -> Program -> Program -> Program
iff (Num c) p q = if c /= 0 then p else q
iff c       p q = If c p q

for :: Expr -> Expr -> (Expr -> Program) -> Program
for (Num a) (Num b) _ | a > b = Skip
for a       b       p         = For a b p

par :: Expr -> Expr -> (Expr -> Program) -> Program
par (Num a) (Num b) _ | a > b = Skip
par a       b       p         = Par a b p

(.>>) :: Program -> Program -> Program
Skip .>> q    = q
p    .>> Skip = p
p    .>> q    = p :>> q

-----------------------------------------------------------------------------
-- locations:

-- A location models what can be on the LHS of an assignment-sign in C.
-- examples: x, arr[17], arr[n][m], etc. In other words, something
-- to which we can write a result.

-- The simplest way to represent a location is a function from result
-- to a program that writes that result to the location.

type Loc a = a -> Program


nil :: Loc a
nil = \_ -> Skip

loc :: Name -> Loc Expr
loc v = \x -> Assign v [] x

(&) :: Loc a -> Loc b -> Loc (a,b)
loc1 & loc2 = \(x,y) -> loc1 x .>> loc2 y

locMap :: (b -> a) -> Loc a -> Loc b
locMap f loc = \x -> loc (f x)

locArray :: Name -> Index -> Loc Expr
locArray v i = \x -> Assign v [i] x


-----------------------------------------------------------------------------
-- Arrays

-- We have two array types, Pull and Push.
-- TODO: explain difference

data Pull a = Pull { pull :: Index -> a }

data Push a = Push { push :: (Index -> Loc a) -> Program }

-- An array is a size and an array type (Pull or Push)
data Array p a =
  Array{ size :: Size
       , doit :: p a
       }

instance Functor Pull where
  fmap f (Pull p) = Pull $ \i -> f (p i)

instance Functor Push where
  fmap f (Push p) = Push $ \iloc -> p (\i -> iloc i . f)

-- primitive (named) arrays
array :: Name -> Size -> Array Pull Expr
array arr n =
  Array{ size = n
       , doit = Pull $ \i -> Index arr [i]
       }

-- Converting any array type to a Push array
class Pushable p where
  toPush :: Array p a -> Array Push a

instance Pushable Push where
  toPush = id

instance Pushable Pull where
  toPush arr =
    Array{ size = size arr
         , doit = Push $ \iloc -> For (Num 0) (size arr) (\i -> iloc i (pull (doit arr) i))
         } 

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



