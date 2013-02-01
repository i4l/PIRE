module Combinators where

-- A small combinator library

import Push
import GenOCL

singleton :: Array Pull Expr
singleton = array "a" (Num 1)

add, mul :: Int -> Int -> Expr

add x y = Num x .+ Num y
mul x y = Num x .* Num y

(??) :: Bool -> (Program, Program) -> Program
True  ?? (p1,p2)  = iff (Num 1) p1 p2
False ?? (p1,p2)  = iff (Num 0) p1 p2


-- Parallelise this via openCl
par :: Int -> (Expr -> Expr) -> Program
par len f = Alloc (Num len) $ \allocF _ -> for (Num 0) (Num len) 
                                               (\e -> allocF e (f e))


fe :: Expr -> Expr
fe e = e .+ e

exPar = par 10 fe
