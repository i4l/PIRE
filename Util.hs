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


nestForAlloc :: Dim -> String -> Type -> [String] -> Dim -> Gen ()
nestForAlloc [] _ _ _ _ = return ()
nestForAlloc [x] name t loopVars acc = return () --l <- fmap fst newLoopVar
                                          --line $ "int " ++ l ++ ";"
                                          --line $ "for( " ++ l ++ " = 0 ; " ++
                                          --       l ++ " < " ++ show x ++ "; " ++ 
                                          --       l ++ "++ ) {"
                                          --indent 2
                                         -- line $ name  ++
                                         --        concat [ "[" ++ i ++ "]" | i <- reverse loopVars] ++ 
                                         --        " = (" ++ 
                                         --        show (typeNest acc t) ++
                                         --        ") malloc(sizeof(" ++
                                         --        show (typeNest (tail acc) t) ++ 
                                         --        ") * " ++ show x ++ ");"
                                          --unindent 2
                                          --line "}"
nestForAlloc (x:xs) name t loopVars acc = do
  l <- fmap fst newLoopVar
  line $ "int " ++ l ++ ";"
  line $ "for( " ++ l ++ " = 0; " ++ l ++ " < " ++ show x ++ "; " ++ l ++ "++ ) {"
  indent 2
  line $ name  ++
         concat [ "[" ++ i ++ "]" | i <- reverse (l:loopVars)] ++ 
         " = (" ++ 
         show (typeNest t (x:acc)) ++
         ") malloc(sizeof(" ++
         show (typeNest t (tail (x:acc))) ++ 
         ") * " ++ show (head xs) ++ ");"
  nestForAlloc xs name t (l:loopVars) (x:acc)
  unindent 2
  line "}"

                                      

------------------------------------------------------------
-- Kernels

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
