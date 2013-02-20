{-# LANGUAGE GADTs #-}
{-
 - PIRE - a Parallel Intermediate Representation for Embedded languages
 - --- Built on a Representation by Koen Lindström Claessen --- 
-}

module PIRE where


-----------------------------------------------------------------------------
-- Expressions

type Name = String

data Expr where
  Num    :: Int -> Expr
  Index  :: Name -> [Expr] -> Expr
  (:+:)  :: Expr -> Expr -> Expr
  (:-:)  :: Expr -> Expr -> Expr
  (:*:)  :: Expr -> Expr -> Expr
  (:/:)  :: Expr -> Expr -> Expr
  (:<=:) :: Expr -> Expr -> Expr

instance Eq Expr where


--data Expr
--  = Num Int
--  | Index Name [Expr]
--  | Expr :+: Expr
--  | Expr :-: Expr
--  | Expr :/: Expr
--  | Expr :*: Expr
--  | Expr :<=: Expr
-- deriving ( Eq )

type Size  = Expr
type Index = Expr 

var :: Name -> Expr
var v = Index v []

-- This instance is quite limited.
instance Ord (Expr) where
  e1 <= e2 = toInt e1<= toInt e2
toInt :: Expr -> Int
toInt (Num n)    = n
toInt (a :-: b)  = toInt a - toInt b
toInt (a :/: b)  = toInt a `div` toInt b
toInt (a :*: b)  = toInt a * toInt b
toInt _          = undefined

instance Show (Expr) where
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

data Program a where
  Skip     :: Program a
  Assign   :: Name -> [Expr] -> Expr -> Program a
  Decl     :: Array Pull a -> Size -> Loc a a -> Program a -- Experimental construct for Declaration
  (:>>)    :: Program a -> Program a -> Program a
  If       :: Expr -> Program a -> Program a -> Program a
  For      :: Expr -> Expr -> (Expr -> Program a) -> Program a
  Par      :: Expr -> Expr -> (Expr -> Program a) -> Program a
  Alloc    :: Size -> ((Index -> Loc (Expr) a) -> Array Pull (Expr) -> Program a) -> Program a
  AllocNew :: Type -> Size -> (Array Pull (Expr)) -> (Loc (Expr) a -> Array Pull (Expr) -> Program a) -> Program a

-- TODO 
--f :: Size -> Loc a -> Program a
alloc :: Array Pull a -> Size -> Loc a a -> Program a
alloc arr s = \loc -> Decl arr s loc

--data Program
--  = Skip
--  | Assign Name [Expr] Expr
--  | Program :>> Program             -- Program Seq.
--  | If Expr Program Program
--  | For Expr Expr (Expr -> Program) -- Sequential Loop
--  | Par Expr Expr (Expr -> Program) -- Parallel Loop
--
----  | ForDim Expr Expr (Array2 Pull Expr) (Loc Expr -> Array2 Pull Expr -> Program) -- TODO experimental!
--
--  | Alloc Size ((Index -> Loc Expr) -> Array Pull Expr -> Program)
--  
--  -- Alloc for multi-dim arrays.
-- -- | AllocDim Type Size (Array2 Pull Expr) (([Index] -> Loc Expr) -> Array2 Pull Expr -> Program) -- TODO experimental!
--
--  | AllocNew Type Size (Array Pull Expr) (Loc Expr        ->
--                                          Array Pull Expr -> 
--                                          Program)
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
-- Types

data Type = TInt | TChar | TFloat | TPointer Type

instance Show Type where
  show TInt = "int"
  show TChar = "char"
  show TFloat = "float"
  show (TPointer t) = show t ++ "*"

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

--type DIM = Int
--
--data Array2 p a = Array2 { arrSize  :: Size
--                         , theData  :: p a
--                         , dim      :: DIM
--                         } 





-- We have two array types, Pull and Push.
-- TODO: explain difference

data Pull a = Pull { pull :: Index -> a }

data Push a = Push { push :: (Index -> Loc a a) -> Program a }

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
array :: Name -> Size -> Array Pull (Expr)
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



