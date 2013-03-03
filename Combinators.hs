{-# LANGUAGE GADTs #-}

module Combinators where

-- A combinator library

import PIRE
import GenOCL
import Util
import Gen

import Array
import Types
import Expr
import Flatten


-----------------------------------------------------------------------------
-- Interface

-- | Initialize an array of length s and type t with function f, followed by the remaining program prog.
initialize :: Type -> Size -> (Index -> Expr) -> Program a -> Program a
initialize t s f prog = Alloc' t s $ \loc -> 
                        for (Num 0) s (\e -> loc e (f e)) -- Initialization loop
                      .>> prog                            -- Followed by the rest of the program


zipWithP :: Type -> Size -> (Expr -> Expr -> Expr) -> Program a
zipWithP t siz f = Alloc' t siz $ \loc' -> loc' (var "XX") $ f (Num 1) (Num 2)

-- adds 1 to each element in the array
test :: Program a
test = initialize (TPointer TInt) len (.+ (Num 45)) $
        zipWithP (TPointer TInt) len (.+)
  where len = Num 10

-- | Parallel zipWith
--zipWithP :: (p ~ Pull) => Type -> (Expr -> Expr -> Expr) -> Array p Expr -> Array p Expr -> Program a
--zipWithP t f arr1 arr2 = AllocNew (TPointer t) len arr1 $ \loc1 kernelArray1 -> 
--                          AllocNew (TPointer t) len arr2 $ \_    kernelArray2 ->
--                            par (Num 0) len $
--                              \e -> loc1 (f (pull (doit kernelArray1) e) (pull (doit kernelArray2) e))
--  where len  = min (size arr1) (size arr2) 
-- 
---- | Parallel map
--mapP :: (p ~ Pull) => Type -> (Expr -> Expr) -> Array p Expr -> Program a
--mapP t f arr = AllocNew (TPointer t) len arr $ 
--                \loc kernelArr -> par (Num 0) len $
--                  \e -> loc (f $ pull (doit kernelArr) e)
--  where len = size arr
--
--
--foo :: Flatten e => Type -> Array Pull e -> Program a
--foo t arr@(Array len (Pull ixf)) = Alloc' t len arr $ \loc iarr -> 
--          for (Num 0) len $ \e -> add1
--

-----------------------------------------------------------------------------
-- Example programs

-- ElementWise vector multiplication
--vecMul :: Program a
--vecMul = zipWithP TInt (.*) vec1 vec2
--  where len  = Num 10
--        vec1 = Array len (Pull (.* (Num 2)))
--        vec2 = Array len (Pull (.+ (Num 1)))
--
---- adds 1 to each element in the array
--add1 :: Program a
--add1 = mapP TInt (.+ (Num 1)) arr
--  where len = Num 10
--        arr = Array len (Pull id)
--
--foo' :: Program a
--foo' = foo TInt arr
--  where len = Num 10
--        arr = Array len (Pull $ \i -> Array len (Pull $ id) )  --2D array




--example :: Gen ()
--example = setupHeadings >> setupOCL >> gen vecMul >> setupPrint "mem1" 10 >> setupEnd

------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ (extractCode prog emptyEnv) ++ (extractCodeK prog emptyEnv)


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
                   writeFile (kernelFile emptyEnv) (unlines $ extractKernelCode prog emptyEnv)






