{-# LANGUAGE GADTs #-}
module Combinators where

-- A small combinator library

import PIRE
import GenOCL
import Util

{- How things works:
 - The arrays passed to dualPar' describe the arrays in the host program, i.e.
 - how the arrays are initialized.

 - The "internal arrays", i.e. those that are arguments in the functions of AllocNew
 - are the ones used in the kernels.
 - These should probably be separated more clearly.
 
 - Note also: quite unspecified behavior when using arrays of differing length.
-}

-- TODO change p ~ Pushable later
dualPar :: (p ~ Pull) => Type -> (Expr -> Expr -> Expr) -> (Array p Expr) -> (Array p Expr) -> Program
dualPar t f arr1 arr2 = AllocNew (TPointer t) len arr1 $ \loc1 kernelArray1 -> 
                          AllocNew (TPointer t) len arr2 $ \_    kernelArray2 ->
                            par (Num 0) len $
                              \e -> loc1 (f (pull (doit kernelArray1) e) (pull (doit kernelArray2) e))
  where len  = min (size arr1) (size arr2) 



vecMul :: Program
vecMul = dualPar TInt (.*) (Array len (Pull (.+ (Num 5)))) (Array len (Pull (.+ (Num 7))))
  where len = Num 20


-- A sequential for-loop program
forProg :: Int -> (Expr -> Expr) -> Program
forProg len f = Alloc  (Num len) $
  \allocf arr -> for (Num 0) (Num len)
                 (\e -> allocf e 
                        (f 
                          (pull (doit arr) (Num 5)) 
                        ) 
                 )


example :: Gen ()
example = setupHeadings >> gen vecMul >> setupEnd




------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ (extractCode prog emptyEnv) ++ (extractCodeK prog emptyEnv)


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
                   writeFile (kernelFile emptyEnv) (unlines $ extractKernelCode prog emptyEnv)

