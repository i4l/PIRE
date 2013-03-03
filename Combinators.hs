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
                        .>> 
                          prog                            -- Followed by the rest of the program


zipWithP :: Type -> Size -> (Expr -> Expr -> Expr) -> Program a
zipWithP t siz f = Alloc' t siz $ \loc' -> loc' (var "XX") $ f (Num 1) (Num 2)

mapP :: Type -> Size -> (Expr -> Expr) -> Program a
mapP t siz f = Alloc' t siz $ \loc' -> for (Num 0) siz $ \e -> loc' e (f (Num 2))

-- adds 1 to each element in the array
test :: Program a
test = initialize (TPointer TInt) len (.+ (Num 45)) $
        mapP (TPointer TInt) len f
  where len = Num 10
        f = (.+ (Num 5))



-----------------------------------------------------------------------------
-- Example programs




--example :: Gen ()
--example = setupHeadings >> setupOCL >> gen vecMul >> setupPrint "mem1" 10 >> setupEnd

------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ (extractCode prog emptyEnv) ++ (extractCodeK prog emptyEnv)


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
                   writeFile (kernelFile emptyEnv) (unlines $ extractKernelCode prog emptyEnv)






