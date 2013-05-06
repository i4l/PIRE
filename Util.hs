module Util where

{- 
 - Utility functions for generating code.
-}
 
import Prelude hiding (GT,LT,EQ)

import Control.Monad.State
import qualified Data.Map as Map

import Program
import Expr
import Types
import Gen

-- | A list of reserved names used in kernels.
reservedNames :: [Name]
reservedNames = ["tid", "ix", "localSize", "globalSize"]

-- | Turns a type into a pointer of the same type. Nests length dim times.
typeNest :: Type -> Dim -> Type
typeNest t = (!!) (iterate TPointer t) . length
--iterate TPointer t !! length dim


-- | Nest 'length xs' number of forloops, where f becomes the innermost program written to p.
nestFor :: Dim -> PartialLoc Expr a -> ([Index] -> Expr) -> [Expr] -> Program a
nestFor []  _     _ _    = Skip
nestFor [x]    inner f vars = for (Num 0) x (\loopvar -> inner (reverse $ loopvar:vars) (f (loopvar:vars)))
nestFor (x:xs) inner f vars = for (Num 0) x (\loopvar -> nestFor xs inner f (loopvar:vars))

-- | Nests for-loops with an inner Program that does not write to a location (e.g. Print programs).
nestFor' :: Dim -> ([Index] -> Program a) -> [Expr] -> Program a
nestFor' [x]    f vars = for (Num 0) x (\loopvar -> f (loopvar:vars))
nestFor' (x:xs) f vars = for (Num 0) x (\loopvar -> nestFor' xs f (loopvar:vars))
--   In case of of a scalar (empty Dim), it allocates a single chunk of type t to lhs.
nestForAlloc :: Dim -> String -> Type -> Gen ()
nestForAlloc [] lhs t  = line $ show t ++ " " ++ lhs ++ " = ("
                      ++ show t ++ ") " ++ "malloc(sizeof(" 
                      ++ case t of
                           TPointer t -> show t
                           t          -> show t
                      ++ "));"
nestForAlloc dim lhs t = do line $ show (typeNest t dim) ++ " " ++ lhs ++ " = (" ++ 
                                            show (typeNest t dim) ++ ") " ++ "malloc(sizeof(" ++ 
                                            show (typeNest t (tail dim)) ++ ")*" ++ showMulExpr dim ++ ");"
                            nest dim lhs t [] []
  where 
    nest [] _ _ _ _  = return ()
    nest [_] _ _ _ _ = return () -- Case needed in order to avoid the last "extra" for-loop.
    nest (x:xs) lhs t loopVars acc = do
      l <- newLoopVar
      line $ "int " ++ l ++ ";"
      line $ "for( " ++ l ++ " = 0; " ++ l ++ " < " ++ show x ++ "; " ++ l ++ "++ ) {"
      indent 2
      line $ lhs  ++
             concat [ "[" ++ i ++ "]" | i <- reverse (l:loopVars)] ++ 
             " = (" ++ 
             show (typeNest t xs) ++
             ") malloc(sizeof(" ++
             show (typeNest t (tail xs)) ++ 
             ") * " ++ show (head xs) ++ ");"
      nest xs lhs t (l:loopVars) (x:acc)
      unindent 2
      line "}"


-- | A Parloop of parloops is transformed to a parloop of forloops.
nestPar :: Dim -> PartialLoc Expr a -> ([Index] -> Expr) -> [Expr] -> Program a
nestPar dim loc f vars | null vars && length dim == 1 = par (Num 0) (head dim) (\loopvar -> loc (reverse $ loopvar:vars) (f (loopvar:vars)))
                       | null vars = par (Num 0) (head dim) (\loopvar -> nestFor (tail dim) loc f (loopvar:vars))
                       | otherwise = error $ "nestPar: vars was: " ++ show vars ++ ". Expected empty list."



seqIf :: Size -> Expr -> (Expr -> Size) -> (Expr -> Program a) -> Program a
seqIf (Num 0) _ _ _    = Skip
seqIf n i condf  prog  = seqIf (n ./ Num 2) (i ./ Num 2) condf prog 
                          .>> iff (condf i) (prog i) Skip

------------------------------------------------------------
-- Kernels

-- remove all pointer wrappings from a Type
removePointers :: Type -> String
removePointers TInt         = "int"
removePointers (TArray t)   = show (TArray t)
--removePointers TChar        = "char"
--removePointers TFloat       = "float"
removePointers (TPointer t) = removePointers t


-- Adds a dereferncing operator (*) to a name iff it is not indexed (i.e. is a scalar).
derefScalar :: Expr -> Expr
--derefScalar a@(Index "tid" _) = a
--derefScalar (Index v []) = deref (Index v [])
derefScalar a@(Index v es) | v `elem` reservedNames = a
                           | not $ null es = a
                           | otherwise     = deref a
derefScalar (Call i@(Index _ _) is)  = Call i (map derefScalar is)
derefScalar (Call i is)  = Call (derefScalar i) (map derefScalar is)
derefScalar (Cond c t f) = Cond (derefScalar c) (derefScalar t) (derefScalar f)
derefScalar (BinOp op)   = BinOp (derefBinOp op)
derefScalar (UnOp op)    = error "derefScalar: UnOp)"
derefScalar a            = a

derefBinOp ::  BOp -> BOp
derefBinOp (Add a b) = Add (derefScalar a) (derefScalar b)
derefBinOp (Sub a b) = Sub (derefScalar a) (derefScalar b)  
derefBinOp (Mul a b) = Mul (derefScalar a) (derefScalar b)  
derefBinOp (Mod a b) = Mod (derefScalar a) (derefScalar b)  
derefBinOp (LT  a b) = LT  (derefScalar a) (derefScalar b)  
derefBinOp (LTE a b) = LTE (derefScalar a) (derefScalar b)  
derefBinOp (GT  a b) = GT  (derefScalar a) (derefScalar b)  
derefBinOp (GTE a b) = GTE (derefScalar a) (derefScalar b)  
derefBinOp (EQ  a b) = EQ  (derefScalar a) (derefScalar b)  
derefBinOp (NEQ a b) = NEQ (derefScalar a) (derefScalar b)  
derefBinOp (And a b) = And (derefScalar a) (derefScalar b)  
derefBinOp (Or  a b) = Or  (derefScalar a) (derefScalar b)
derefBinOp (BWAnd  a b) = BWAnd  (derefScalar a) (derefScalar b)
derefBinOp (BWOr  a b)  = BWOr  (derefScalar a) (derefScalar b)
derefBinOp (BWXOr a b)  = BWXOr  (derefScalar a) (derefScalar b)
derefBinOp (ShiftL a b) = ShiftL  (derefScalar a) (derefScalar b)
derefBinOp (ShiftR a b) = ShiftR  (derefScalar a) (derefScalar b)




