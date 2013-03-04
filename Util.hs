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
