-----------------------------------------------------------------------------
-- | Module providing AST anaysis for various tasks
-----------------------------------------------------------------------------
module Analysis (Parameters, grabKernelParams) where

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
grabKernelParams' (If c tb fb) = let cond   = exprAsParam c 
                                     bodies = grabKernelParams $ tb :>> fb
                                 in cond ++ bodies
grabKernelParams' (For start end f) = grabKernelParams $ f (var "tid")
grabKernelParams' (Par start end f) = error "par"
grabKernelParams' (Alloc t dim p)   = error "alloc"
grabKernelParams' (Print t e)       = error "print"
grabKernelParams' _                 = []

-- | Extracts names, types ('bit iffy atm) and type of all array Indexing operations.
exprAsParam :: Expr -> Parameters
exprAsParam (Index a is) = if length is > 0 then [(a,is, typeNest TInt is)] else []
exprAsParam (a :+: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :-: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :/: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :%: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :*: b)    = exprAsParam a ++ exprAsParam b
exprAsParam (a :<=: b)   = exprAsParam a ++ exprAsParam b
exprAsParam (a :==: b)   = exprAsParam a ++ exprAsParam b
exprAsParam _            = []

-----------------------------------------------------------------------------

