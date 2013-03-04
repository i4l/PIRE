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
initialize :: Type -> [Size] -> ([Index] -> Expr) -> (IndexedArray -> Program a) -> Program a
initialize t dim f prog = Alloc t dim $ \partialLoc arrayName -> 
                            nestFor dim partialLoc f []   -- Build a nesting of for-loops
                          .>>
                            prog arrayName                -- Followed by the rest of the program

nestFor :: [Size] -> PartialLoc Expr a -> ([Index] -> Expr) -> [Expr] -> Program a
nestFor []  _     _ _    = Skip
nestFor [x] inner f vars = for (Num 0) x (\loopvar -> inner (reverse $ loopvar:vars) (f (loopvar:vars)))
nestFor (x:xs) p  f vars = for (Num 0) x (\loopvar -> nestFor xs p f (loopvar:vars))


mapP :: Type -> [Size] -> IndexedArray -> (Expr -> Expr) -> Program a
mapP t dim arr f = Alloc t dim $ \loc' _ -> for (Num 0) (head dim) $ \e -> loc' [e] (f $ arr [e])




-----------------------------------------------------------------------------
-- Example programs

-- | With initialize and mapP helper functions.
mapTest :: Program a
mapTest = initialize t dim initf $
         \arrName -> mapP t dim arrName apply
  where dim = [Num 10,Num 7, Num 5]
        t = TInt 
        initf xs = (.+ Num 3) $ (xs !! 0) .* (xs !! 2) -- foldr1 (.*) xs
        apply = (.+ Num 5)


-- | Without initialize and mapP
--mapTest' :: Program a
--mapTest' = Alloc t [len] $ \partialLoc arrName1 ->
--                      for (Num 0) len (\e -> partialLoc [e] (initf e))
--                      .>>
--                        Alloc t [len] $ \loc' _ ->
--                          for (Num 0) len $ \e -> loc' [e] (apply $ arrName1 [e])
--  where len = Num 10
--        t = TPointer TInt
--        initf = (.* Num 3)
--        apply = (.+ Num 5)


------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ extractCode prog emptyEnv ++ extractCodeK prog emptyEnv


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
                   writeFile (kernelFile emptyEnv) (unlines $ extractKernelCode prog emptyEnv)






