{-# LANGUAGE GADTs #-}

module Combinators where

-- A combinator library

import Program
import GenOCL
import Util
import Gen

import Types
import Expr


-----------------------------------------------------------------------------
-- Interface

-- | Initialize an array of length s and type t with function f, followed by the remaining program prog.
initArray :: Type -> Dim -> ([Index] -> Expr) -> (IndexedArray -> Program a) -> Program a
initArray t dim f prog = Alloc t dim $ \partialLoc arrayName -> 
                            nestFor dim partialLoc f []   -- Build a nesting of for-loops
                          .>>
                            prog arrayName                -- Followed by the rest of the program

-- TODO this might be a bit off.
initScalar :: Type -> Expr -> Program a -> Program a
initScalar t e prog = Alloc t [] $ \partialLoc _ -> (partialLoc [Num 0] e)



-- | Experimental map (to have something to play around with).
mapP :: Type -> Dim -> ([Index] -> Expr) -> IndexedArray -> Program a
mapP t dim arr f = Alloc t dim $ \partialLoc _ -> 
                      nestFor dim partialLoc (\xs -> f [arr $ reverse xs]) [] 

-- | sequential scanl on 1D array using f.
scan :: Type -> Dim -> (Expr -> Expr -> Expr) -> IndexedArray -> Program a
scan t dim f arr = Alloc t [head dim .+ Num 1] $ \partialLoc iarr -> 
                      for (Num 1) (head dim .+ Num 1) $ \e -> partialLoc 
                                                                [e] 
                                                                (f (iarr [e] .- Num 1) 
                                                                (arr [e .- Num 1]))


-----------------------------------------------------------------------------
-- Example programs

-- | With initialize and mapP helper functions.
mapTest :: Program a
mapTest = initArray t dim initf $
         \arrName -> mapP t dim arrName apply
  where dim = [Num 10]
        t = TInt 
        initf xs = (Num 3 .+) $ foldr1 (.*) xs --(.+ Num 3) $ (xs !! 0) .* (xs !! 2) -- foldr1 (.*) xs
        apply xs = xs !! 0 .+ Num 5

scanTest :: Program a
scanTest = initArray t dim initf $
              \arrName -> scan t dim apply arrName
  where dim = [Num 10]
        t = TInt 
        initf xs = (Num 3 .+) $ foldr1 (.*) xs --(.+ Num 3) $ (xs !! 0) .* (xs !! 2) -- foldr1 (.*) xs
        apply e1 e2 = e1 .+ e2

example :: Gen ()
example = setupHeadings >> gen scanTest >> setupEnd

------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ extractCode prog emptyEnv ++ extractCodeK prog emptyEnv


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
                   writeFile (kernelFile emptyEnv) (unlines $ extractKernelCode prog emptyEnv)






