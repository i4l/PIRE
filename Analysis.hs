-----------------------------------------------------------------------------
-- | Module providing AST anaysis for various tasks
-----------------------------------------------------------------------------
module Analysis (  Parameters
                 , grabKernelParams
                 , parForUnwind
                 , isParallel
                 , removeDupBasicProg
                 ) where

import Util
import Expr
import Program
import Types

import Data.List
import Control.Monad

-----------------------------------------------------------------------------
-- kernel parameters

type Parameters = [(Name, Dim, Type)]

-- | grabKernelParams p gets the arrays used in p as a list of parameters that can be used in a kernel.
--   Removes duplicates by name only.
grabKernelParams :: Program a -> Parameters
grabKernelParams = rmDup . grabKernelParams' 
  where rmDup      = nubBy $ \(n1,_,_) (n2,_,_) -> n1 == n2

grabKernelParams' :: Program a -> Parameters
grabKernelParams' (Assign name es e) = let lhs = (name,es,typeNest TInt es) -- TODO: hard-coded to Int.
                                           rhs = exprAsParam e
                                       in (lhs:rhs)
grabKernelParams' (a :>> b) = grabKernelParams a ++ grabKernelParams b
grabKernelParams' (If c t f) = let cond   = exprAsParam c 
                                   bodies = grabKernelParams $ t :>> f
                               in cond ++ bodies
grabKernelParams' (For start end f) = grabKernelParams $ f (var "tid")
grabKernelParams' (BasicProc p)     = grabKernelParams p
grabKernelParams' OutParam{}        = error "OutParam in grabKernelParams'"
grabKernelParams' InParam{}         = error "InParam in grabKernelParams'"
grabKernelParams' (Par start end f) = error "par"
grabKernelParams' (Alloc t dim p)   = error "alloc"
grabKernelParams' (Print t e)       = error "print"
grabKernelParams' _                 = []

-- | Extracts names and types array Indexing operations.
exprAsParam :: Expr -> Parameters
exprAsParam (Index a is) = [(a, is, typeNest TInt is)]
exprAsParam (Call a is)  = exprAsParam a ++ (concat $ map exprAsParam is)
exprAsParam (a :+: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :-: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :/: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :%: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :*: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :<=: b)   = exprAsParam a ++ exprAsParam b
exprAsParam (a :==: b)   = exprAsParam a ++ exprAsParam b
exprAsParam _            = []

-----------------------------------------------------------------------------
-- ParFor unwinding

parForUnwind :: Program a -> Name -> Program a
parForUnwind (Par start end f) new = parForUnwind (For start end f) new
parForUnwind (For start end f) new = f $ var new
parForUnwind p                 new = p

-----------------------------------------------------------------------------
-- Are we compiling for OpenCL or regular C (so we know if we should add the OpenCL boilerplate block)

isParallel :: Program a -> Bool
isParallel (a :>> b)      = isParallel a || isParallel b
isParallel (If _ t f)     = isParallel t || isParallel f
isParallel (For _ _ f)    = isParallel $ f (var "x")
isParallel (Par _ _ _)    = True
isParallel (Alloc _ _ f)  = isParallel $ f "x"
isParallel (BasicProc p)  = isParallel p
isParallel (OutParam t f) = isParallel $ f "out"
isParallel (InParam t f)  = isParallel $ f "arg"
isParallel _              = False

-----------------------------------------------------------------------------
-- Remove duplicate BasicProc

removeDupBasicProg :: Program a -> Program a
removeDupBasicProg (a :>> b)      = removeDupBasicProg a :>> removeDupBasicProg b
removeDupBasicProg (If c t f)     = iff c (removeDupBasicProg t) (removeDupBasicProg f)
removeDupBasicProg (BasicProc p)  = p
removeDupBasicProg (For a b f)    = for a b $ \e -> removeDupBasicProg $ f e
removeDupBasicProg (Par a b f)    = par a b $ \e -> removeDupBasicProg $ f e
removeDupBasicProg (Alloc t dim f)= Alloc t dim $ \name -> removeDupBasicProg $  f name
removeDupBasicProg (OutParam t f) = OutParam t $ \name -> removeDupBasicProg $ f name
removeDupBasicProg (InParam t f)  = InParam t $ \name -> removeDupBasicProg $ f name
removeDupBasicProg p              = p

