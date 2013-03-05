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
initialize :: Type -> Dim -> ([Index] -> Expr) -> (IndexedArray -> Program a) -> Program a
initialize t dim f prog = Alloc t dim $ \partialLoc arrayName -> 
                            nestFor dim partialLoc f []   -- Build a nesting of for-loops
                          .>>
                            prog arrayName                -- Followed by the rest of the program

-- | Nest 'length xs' number of forloops, where f becomes the innermost program written to p.
nestFor :: Dim -> PartialLoc Expr a -> ([Index] -> Expr) -> [Expr] -> Program a
nestFor []  _     _ _    = Skip
nestFor [x] inner f vars = for (Num 0) x (\loopvar -> inner (reverse $ loopvar:vars) (f (loopvar:vars)))
nestFor (x:xs) p  f vars = for (Num 0) x (\loopvar -> nestFor xs p f (loopvar:vars))

-- | Experimental map.
mapP :: Type -> Dim -> ([Index] -> Expr) -> IndexedArray -> Program a
mapP t dim arr f = Alloc t dim $ \partialLoc _ -> 
                      nestFor dim partialLoc (\xs -> f [arr $ reverse xs]) [] 




-----------------------------------------------------------------------------
-- Example programs

-- | With initialize and mapP helper functions.
mapTest :: Program a
mapTest = initialize t dim initf $
         \arrName -> mapP t dim arrName apply
  where dim = [Num 100]
        t = TInt 
        initf xs = foldr1 (.*) xs --(.+ Num 3) $ (xs !! 0) .* (xs !! 2) -- foldr1 (.*) xs
        apply xs = xs !! 0 .+ Num 5


example :: Gen ()
example = setupHeadings >> gen mapTest >> setupEnd

------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ extractCode prog emptyEnv ++ extractCodeK prog emptyEnv


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
                   writeFile (kernelFile emptyEnv) (unlines $ extractKernelCode prog emptyEnv)






