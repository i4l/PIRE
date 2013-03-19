-----------------------------------------------------------------------------
-- | Module providing AST anaysis for various tasks
-----------------------------------------------------------------------------
module Analysis where

import Util
import Expr
import Program
import Types

-----------------------------------------------------------------------------
-- Get the arrays to be used as kernel parameters

data KData = KData {params :: [(Name, Dim, Type)]}
--type Parameters = [(Name, Dim, Type)]

grabKernelParams :: Program a -> KData
grabKernelParams (Assign name es e) = let lhs = (name,es,typeNest TInt es)
                                          rhs = getKDataExpr e
                                      in (KData $ lhs:( params rhs))
grabKernelParams (a :>> b) = grabKernelParams a +++ grabKernelParams b
  where a +++ b = KData $ (params a) ++ (params b)
grabKernelParams (If c tb fb) = let cond   = getKDataExpr c 
                                    bodies = grabKernelParams $ tb :>> fb
                                in KData $  params cond ++ params bodies
grabKernelParams (For start end f) = grabKernelParams $ f (var "tid")
grabKernelParams (Par start end f) = error "par"
grabKernelParams (Alloc t dim p)   = error "alloc"
grabKernelParams (Print t e)       = error "print"
grabKernelParams _                 = KData []

getKDataExpr :: Expr -> KData
getKDataExpr (Index a is) = KData [(a,is, typeNest TInt is)]
getKDataExpr (a :+: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :-: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :/: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :%: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :*: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :<=: b)   = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr _            = KData []


