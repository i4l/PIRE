{-# LANGUAGE GADTs #-}

module Expr where

import Prelude hiding (LT, EQ, GT)

type Name = String

-- | A Dimension is simply a list of Expressions.
type Dim = [Size]

type Size  = Expr
type Index = Expr 

-- | Memory locations
data Memory = Host
            | DevGlobal
            | DevLocal
  deriving Eq

-- | Expressions are the RHS in an assignment.
data Expr where
  Num    :: Int -> Expr
  Index  :: Memory -> Name -> [Expr] -> Expr
  Call   :: Expr -> [Expr] -> Expr
  Cond   :: Expr -> Expr -> Expr -> Expr
  BinOp  :: BOp -> Expr
  UnOp   :: UOp -> Expr
  deriving Eq


-- | Unary operators
data UOp where
  BWNeg  :: Expr -> UOp
  Deref  :: Expr -> UOp
  deriving Eq

-- | Binary operators
data BOp where
  Add     :: Expr -> Expr -> BOp 
  Sub     :: Expr -> Expr -> BOp 
  Mul     :: Expr -> Expr -> BOp 
  Div     :: Expr -> Expr -> BOp 
  Mod     :: Expr -> Expr -> BOp 
  LT      :: Expr -> Expr -> BOp 
  LTE     :: Expr -> Expr -> BOp
  GT      :: Expr -> Expr -> BOp 
  GTE     :: Expr -> Expr -> BOp
  EQ      :: Expr -> Expr -> BOp
  NEQ     :: Expr -> Expr -> BOp
  And     :: Expr -> Expr -> BOp
  Or      :: Expr -> Expr -> BOp
  BWAnd   :: Expr -> Expr -> BOp
  BWOr    :: Expr -> Expr -> BOp
  BWXOr   :: Expr -> Expr -> BOp
  ShiftL  :: Expr -> Expr -> BOp
  ShiftR  :: Expr -> Expr -> BOp
  deriving Eq 


instance Show UOp where
  show (BWNeg a) = "(~" ++ show a ++ ")"
  show (Deref a) = "(*" ++ show a ++ ")"

instance Show BOp where
  show (Add a b) = "(" ++ show a ++ " + " ++ show b ++ ")" 
  show (Sub a b) = "(" ++ show a ++ " - " ++ show b ++ ")" 
  show (Mul a b) = "(" ++ show a ++ " * " ++ show b ++ ")" 
  show (Mod a b) = "(" ++ show a ++ " % " ++ show b ++ ")" 
  show (LT  a b) = "(" ++ show a ++ " < " ++ show b ++ ")" 
  show (LTE a b) = "(" ++ show a ++ " <= " ++ show b ++ ")" 
  show (GT  a b) = "(" ++ show a ++ " > " ++ show b ++ ")" 
  show (GTE a b) = "(" ++ show a ++ " >= " ++ show b ++ ")" 
  show (EQ  a b) = "(" ++ show a ++ " == " ++ show b ++ ")" 
  show (NEQ a b) = "(" ++ show a ++ " != " ++ show b ++ ")" 
  show (And a b) = "(" ++ show a ++ " && " ++ show b ++ ")" 
  show (Or  a b) = "(" ++ show a ++ " || " ++ show b ++ ")"
  show (BWAnd  a b) = "(" ++ show a ++ " & " ++ show b ++ ")"
  show (BWOr   a b) = "(" ++ show a ++ " | " ++ show b ++ ")"
  show (BWXOr   a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
  show (ShiftL  a b) = "(" ++ show a ++ " << " ++ show b ++ ")"
  show (ShiftR  a b) = "(" ++ show a ++ " >> " ++ show b ++ ")"




-- | create a 'scalar' variable
var :: Name -> Expr
var v = Index Host v []

glob :: Name -> Expr
glob v = Index DevGlobal v []

deref :: Expr -> Expr
deref a = UnOp (Deref a)

-- | Converts an Expr to an Int, albeit in a limited fashion. 
toInt :: Expr -> Int
toInt (Num n)    = n
--toInt (a :-: b)  = toInt a - toInt b
--toInt (a :/: b)  = toInt a `div` toInt b
--toInt (a :*: b)  = toInt a * toInt b
--toInt _          = undefined

nameFromVar :: Expr -> Name
nameFromVar (Index _ v _) = v
nameFromVar x           = error "expected Index but got " ++ show x

instance Ord (Expr) where
  e1 <= e2 = toInt e1 <= toInt e2

instance Show Expr where
  show (Num n)       = show n
  show (Index _ a is)  = a ++ concat [ "[" ++ show i ++ "]" | i <- is ]
  show (Call e args) = show e ++ "(" ++ if null as then ")" else (init . concat) as ++ ")"
    where as = [show a ++ "," | a <- args]
  show (BinOp op) = show op
  show (UnOp  op) = show op
  show (Cond c a b) = show c ++ " ? " ++ show a ++ " : " ++ show b

-- | Reduce a list of Expr to a single Expr as a string.
showMulExpr :: [Expr] -> String
showMulExpr = show . foldr1 (.*)

-----------------------------------------------------------------------------
-- "Smart" Constructors for expressions

--(.+), (.-), (.<=), (./), (.%), (.*) :: Expr -> Expr -> Expr
Num 0 .+ b     = b
a     .+ Num 0 = a
Num a .+ Num b = Num (a+b)
a     .+ b     = BinOp $ Add a b

Num a .- Num b = Num (a-b)
a     .- Num 0 = a
a     .- b     = BinOp $ Sub a b

Num a ./ Num b = Num (a `div` b)
Num 0 ./ b     = Num 0
a     ./ Num 1 = a
a     ./ b     = BinOp $ Div a b

Num a .% Num b = Num (a `mod` b)
a     .% Num 1 = Num 0
a     .% b     = BinOp $ Mod a b

Num a .* Num b = Num (a*b)
a     .* Num 0 = Num 0
a     .* Num 1 = a
Num 0 .* b     = Num 0
Num 1 .* b     = b
a     .* b     = BinOp $ Mul a b
--
--Num a .<= Num b      = Num (if a<=b then 1 else 0)
--a     .<= b | a == b = Num 1
--a     .<= b          = a :<=: b
--
--Num a .== Num b      = Num (if a==b then 1 else 0)
--a     .== b          = a :==: b
