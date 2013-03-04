{-# LANGUAGE GADTs #-}

module Expr where

type Name = String


-- | Expressions are the RHS in an assignment.
data Expr where
  Num    :: Int -> Expr
  Index  :: Name -> [Expr] -> Expr
  (:+:)  :: Expr -> Expr -> Expr
  (:-:)  :: Expr -> Expr -> Expr
  (:*:)  :: Expr -> Expr -> Expr
  (:/:)  :: Expr -> Expr -> Expr
  (:<=:) :: Expr -> Expr -> Expr

instance Eq Expr where

type Size  = Expr
type Index = Expr 

-- | create a 'scalar' variable
var :: Name -> Expr
var v = Index v []

-- | Converts an Expr to an Int, albeit in a limited fashion. 
toInt :: Expr -> Int
toInt (Num n)    = n
toInt (a :-: b)  = toInt a - toInt b
toInt (a :/: b)  = toInt a `div` toInt b
toInt (a :*: b)  = toInt a * toInt b
toInt _          = undefined

instance Ord (Expr) where
  e1 <= e2 = toInt e1<= toInt e2

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
