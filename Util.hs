module Util where

{- 
 - Utility functions for generating code.
-}
 
import Prelude hiding (GT,LT,EQ)

import Control.Monad.State
import qualified Data.Map as Map
import Data.List

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


-- Adds a dereferncing operator (*) to a name iff it is not indexed.
-- List of Names describe names to exclude (not dereference).
derefScalar :: Expr -> [Name] -> Expr
derefScalar a@(Index m v es) ns | v `elem` (reservedNames ++ nub ns) = Index m v (map (flip derefScalar ns) es)
                                | not $ null es = Index m v (map (flip derefScalar ns) es)
                                | otherwise     = deref a
derefScalar (Call i@(Index _ _ _) is) ns  = Call i $ map (flip derefScalar ns) is
derefScalar (Call i is)  ns = Call (derefScalar i ns) $ map (flip derefScalar ns) is
derefScalar (Cond c t f) ns = Cond (derefScalar c ns) (derefScalar t ns) (derefScalar f ns)
derefScalar (BinOp op)   ns = BinOp (derefBinOp op ns)
derefScalar (UnOp op)    ns = error "derefScalar: UnOp"
derefScalar a            ns = a

derefBinOp ::  BOp -> [Name] -> BOp
derefBinOp (Add a b) ns = Add (derefScalar a ns) (derefScalar b ns)
derefBinOp (Sub a b) ns = Sub (derefScalar a ns) (derefScalar b ns)  
derefBinOp (Mul a b) ns = Mul (derefScalar a ns) (derefScalar b ns)  
derefBinOp (Mod a b) ns = Mod (derefScalar a ns) (derefScalar b ns)  
derefBinOp (LT  a b) ns = LT  (derefScalar a ns) (derefScalar b ns)  
derefBinOp (LTE a b) ns = LTE (derefScalar a ns) (derefScalar b ns)  
derefBinOp (GT  a b) ns = GT  (derefScalar a ns) (derefScalar b ns)  
derefBinOp (GTE a b) ns = GTE (derefScalar a ns) (derefScalar b ns)  
derefBinOp (EQ  a b) ns = EQ  (derefScalar a ns) (derefScalar b ns)  
derefBinOp (NEQ a b) ns = NEQ (derefScalar a ns) (derefScalar b ns)  
derefBinOp (And a b) ns = And (derefScalar a ns) (derefScalar b ns)  
derefBinOp (Or  a b) ns = Or  (derefScalar a ns) (derefScalar b ns)
derefBinOp (BWAnd  a b) ns = BWAnd  (derefScalar a ns) (derefScalar  b ns)
derefBinOp (BWOr  a b)  ns = BWOr  (derefScalar a ns) (derefScalar   b ns)
derefBinOp (BWXOr a b)  ns = BWXOr  (derefScalar a ns) (derefScalar  b ns)
derefBinOp (ShiftL a b) ns = ShiftL  (derefScalar a ns) (derefScalar b ns)
derefBinOp (ShiftR a b) ns = ShiftR  (derefScalar a ns) (derefScalar b ns)




