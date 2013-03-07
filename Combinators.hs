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
initScalar :: Type -> Expr -> (PartialLoc Expr a -> IndexedArray -> Program a) -> Program a
initScalar t e prog = Alloc t [] $ \partialLoc arr -> partialLoc [Num 0] e .>> prog partialLoc arr

-- | Prints an array arr of type t and size s.
--  TODO currently doesn't take type into consideration. Defaults to Int
printArray :: Type ->  Size -> IndexedArray -> Program a
printArray t s arr = for (Num 0) s $ \e -> Print t $ arr [e]

-- | Experimental map (to have something to play around with).
mapP :: Type -> Dim -> ([Index] -> Expr) -> IndexedArray -> Program a
mapP t dim arr f = Alloc t dim $ \partialLoc _ -> 
                      nestFor dim partialLoc (\xs -> f [arr $ reverse xs]) [] 

-- | sequential scanl on 1D array using f.
scan :: Type -> Dim -> (Expr -> Expr -> Expr) -> IndexedArray -> (IndexedArray -> Program a) -> Program a
scan t dim f arr prog = Alloc t [head dim .+ Num 1] $ \partialLoc iarr -> 
                          for (Num 1) (head dim .+ Num 1) 
                              (\e -> partialLoc [e] (f (iarr [e] .- Num 1) (arr [e .- Num 1])))
                      .>> prog iarr

-- | sequential foldl on 1D array using f.
fold :: Type -> Size -> (Expr -> Expr -> Expr) -> Expr -> IndexedArray -> (IndexedArray -> Program a) -> Program a
fold t s f acc arr prog = initScalar t acc $ \loc iarr -> 
                            for (Num 0) s (\e -> loc [Num 0] (f (iarr [Num 0]) (arr [e])))
                        .>> prog iarr


-----------------------------------------------------------------------------
-- Example programs

-- | With initialize and mapP helper functions.
--mapTest :: Program a
--mapTest = initArray t dim initf $
--         \arrName -> mapP t dim arrName apply
--  where dim = [Num 10]
--        t = TInt 
--        initf xs = (Num 3 .+) $ foldr1 (.*) xs --(.+ Num 3) $ (xs !! 0) .* (xs !! 2) -- foldr1 (.*) xs
--        apply xs = xs !! 0 .+ Num 5
--
--scanTest :: Program a
--scanTest = initArray t dim initf $
--              \arrName -> scan t dim apply arrName
--  where dim = [Num 10]
--        t = TInt 
--        initf xs = (Num 3 .+) $ foldr1 (.*) xs --(.+ Num 3) $ (xs !! 0) .* (xs !! 2) -- foldr1 (.*) xs
--        apply e1 e2 = e1 .+ e2

foldTest :: Program a
foldTest = initArray t dim initf $
              \arrName -> fold t (head dim) apply acc arrName $
              \foldedName -> printArray t (Num 1) foldedName
  where dim = [Num 10]
        acc = Num 0
        t = TInt 
        initf xs = (Num 3 .+) $ foldr1 (.*) xs --(.+ Num 3) $ (xs !! 0) .* (xs !! 2) -- foldr1 (.*) xs
        apply = (.+)

example :: Gen ()
example = setupHeadings >> gen foldTest >> setupEnd

------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ extractCode prog emptyEnv ++ extractCodeK prog emptyEnv


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
                   writeFile (kernelFile emptyEnv) (unlines $ extractKernelCode prog emptyEnv)






