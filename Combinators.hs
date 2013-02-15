{-# LANGUAGE GADTs #-}

module Combinators where

-- A small combinator library

import PIRE
import GenOCL
import Util



-----------------------------------------------------------------------------
-- Building blocks

{- How things works:
 - The arrays passed to dualPar' describe the arrays in the host program, i.e.
 - how the arrays are initialized.

 - The "internal arrays", i.e. those that are arguments in the functions of AllocNew
 - are the ones used in the kernels.
 - These should probably be separated more clearly.
 
 - Note also: quite unspecified behavior when using arrays of differing length.
-}

-- TODO change p ~ Pushable later


parLoop2 :: (p ~ Pull) => Type -> (Expr -> Expr -> Expr) -> Array p Expr -> Array p Expr -> Program
parLoop2 t f arr1 arr2 = AllocNew (TPointer t) len arr1 $ \loc1 kernelArray1 -> 
                          AllocNew (TPointer t) len arr2 $ \_    kernelArray2 ->
                            par (Num 0) len $
                              \e -> loc1 (f (pull (doit kernelArray1) e) (pull (doit kernelArray2) e))
  where len  = min (size arr1) (size arr2) 

parLoop :: (p ~ Pull) => Type -> (Expr -> Expr) -> (Array p Expr) -> Program
parLoop t f arr = AllocNew (TPointer t) (size arr) arr $ \loc1 kernelArray -> 
                        par (Num 0) (size arr) $
                          \e -> loc1 (f (pull (doit kernelArray) e) )

-- A sequential for-loop program
forProg :: Int -> (Expr -> Expr) -> Program
forProg len f = Alloc (Num len) $
  \allocf arr -> for (Num 0) (Num len) $ 
                  \e -> allocf e $ f e



parLoop' :: Type -> (Expr -> Expr) -> Array Pull Expr -> Program
parLoop' t f arr = AllocNew t (size arr) arr $
                      \loc internalArr -> undefined

--parLoop2Nest :: (p ~ Pull) => Type -> (Expr -> Expr -> Expr) -> (Array p Expr) -> (Array p Expr) -> Program
--parLoop2Nest t f arr1 arr2 = AllocNew (TPointer t) len arr1 $ \loc1 kernelArray1 -> 
--                              AllocNew (TPointer t) len arr2 $ \loc2 kernelArray2 ->
--                                par (Num 0) len $
--                                  \e -> locNest [e] (f (pull (doit kernelArray1) e) (pull (doit kernelArray2) e))
--      where len  = min (size arr1) (size arr2) 
-----------------------------------------------------------------------------
-- Example programs

a = parLoop' TInt id (Array (Num 10) (Pull id))
 

-- Vector multiplication
vecMul :: Program
vecMul = parLoop2 TInt (.*) vec1 vec2
  where len  = Num 10
        vec1 = Array len (Pull (.* (Num 2)))
        vec2 = Array len (Pull (.+ (Num 1)))



--forLoopNest :: (a -> a) -> Array Pull a -> Program
--forLoopNest f (Array len (Pull ixf)) = for (Num 0) len $
--                                            \e -> Assign "var" [] (ixf $ f e)


forLoop' :: (Expr -> Expr) -> Array2 Pull Expr -> Program  
forLoop' f arr@(Array2 len (Pull ixf) dim) = ForDim (Num 0) len arr $ 
                                          \loc internalArr -> loc $ (pull $ theData internalArr) (var "v")
                                                          
b = forLoop' (.+ (Num 5)) (Array2 (Num 10) (Pull (.* (Num 4))) 2)

--forLoop' :: (Expr -> Expr) -> Array2 Pull Expr -> Program
--forLoop' f (Array2 len (Pull ixf) dim) = ForDim len dim $
--                                          \e -> Assign "arr" [] $ ixf e




inc1 :: Program
inc1 = forProg 10 (.+ (Num 1))

inc1Par :: Gen ()
inc1Par = gen $ parLoop TInt (.+ (Num 1)) (Array (Num 15) (Pull id))

example :: Gen ()
example = setupHeadings >> setupOCL >> gen vecMul >> setupPrint "mem1" 10 >> setupEnd




------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ (extractCode prog emptyEnv) ++ (extractCodeK prog emptyEnv)


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
                   writeFile (kernelFile emptyEnv) (unlines $ extractKernelCode prog emptyEnv)

