module Util where

{- 
 - Utility functions for generating code.
-}
 
import Control.Monad.State
import qualified Data.Map as Map

import Program
import Expr
import Types
import Gen

-- | Turns a type into a pointer of the same type. Nests length dim times.
typeNest :: Type -> Dim -> Type
typeNest t = (!!) (iterate TPointer t) . length
--iterate TPointer t !! length dim


-- | Nest 'length xs' number of forloops, where f becomes the innermost program written to p.
nestFor :: Dim -> PartialLoc Expr a -> ([Index] -> Expr) -> [Expr] -> Program a
nestFor []  _     _ _    = Skip
nestFor [x] inner f vars = for (Num 0) x (\loopvar -> inner (reverse $ loopvar:vars) (f (loopvar:vars)))
nestFor (x:xs) p  f vars = for (Num 0) x (\loopvar -> nestFor xs p f (loopvar:vars))


-- | Nests for-loops used for allocating an array of the dimension given by first Dim.
--   In case of of a scalar (empty Dim), it allocates a single chunk of type t to lhs.
nestForAlloc :: Dim -> String -> Type -> Gen ()
nestForAlloc [] lhs t  = line $ show (TPointer t) ++ " " ++ lhs ++ " = ("
                      ++ show (TPointer t) ++ ") " ++ "malloc(sizeof(" 
                      ++ show t ++ "));"
nestForAlloc dim lhs t = do line $ show (typeNest t dim) ++ " " ++ lhs ++ " = (" ++ 
                                            show (typeNest t dim) ++ ") " ++ "malloc(sizeof(" ++ 
                                            show (typeNest t (tail dim)) ++ ")*" ++ showMulExpr dim ++ ");"
                            nest dim lhs t [] []
  where 
    nest [] _ _ _ _  = return ()
    nest [_] _ _ _ _ = return () -- Case needed in order to avoid the last "extra" for-loop.
    nest (x:xs) lhs t loopVars acc = do
      l <- fmap fst newLoopVar
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



nestPar :: Dim -> PartialLoc Expr a -> ([Index] -> Expr) -> [Expr] -> Program a
nestPar dim p f vars | null vars = par (Num 0) (head dim) (\loopvar -> nestFor (tail dim) p f (loopvar:vars))
                     | otherwise = error $ "nestPar: vars was: " ++ show vars ++ ". Expected empty list."
--                     | not (null vars) = nestFor dim p f vars

--nestPar []  _     _ _    = Skip
--nestPar [x] inner f vars = for (Num 0) x (\loopvar -> inner (reverse $ loopvar:vars) (f (loopvar:vars)))
--nestPar (x:xs) p  f vars = for (Num 0) x (\loopvar -> nestFor xs p f (loopvar:vars))                                     

------------------------------------------------------------
-- Kernels

data KData = KData
                {params :: [(Name, Dim, Type)]
                }
                   
getKernelFile :: Gen String
getKernelFile = gets kernelFile


lineK :: String -> Gen ()
lineK s = modify $ \env -> env {kernelCode = kernelCode env ++ [s]}

extractCodeK :: Gen a -> Env -> [String]
extractCodeK g e = kernelCode $ execState g e

-- other

-- remove all pointer wrappings from a Type
removePointer :: Type -> String
removePointer TInt         = "int"
removePointer (TArray t)   = show (TArray t)
--removePointer TChar        = "char"
--removePointer TFloat       = "float"
removePointer (TPointer t) = removePointer t
