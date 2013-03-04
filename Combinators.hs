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
initialize :: Type -> [Size] -> (Index -> Expr) -> (IndexedArray -> Program a) -> Program a
initialize t dim f prog = Alloc t dim $ \partialLoc arrayName -> 
                            nestFor dim partialLoc 

                         --for (Num 0) (head dim) (\e -> partialLoc [e] $ f e) -- Initialization loop
                       -- .>> 
                       --   prog arrayName                            -- Followed by the rest of the program

nestFor :: [Size] -> PartialLoc Expr a -> Program a
nestFor [] _      = Skip
nestFor [x] inner = for (Num 0) x (\e -> inner [e] e)
nestFor (x:xs) p  = for (Num 0) x (\_ -> nestFor xs p)


mapP :: Type -> [Size] -> IndexedArray -> (Expr -> Expr) -> Program a
mapP t dim arr f = Alloc t dim $ \loc' _ -> for (Num 0) (head dim) $ \e -> loc' [e] (f $ arr [e])




-----------------------------------------------------------------------------
-- Example programs

-- | With initialize and mapP helper functions.
mapTest :: Program a
mapTest = initialize t dim initf $
         \arrName -> mapP t dim arrName apply
  where dim = [Num 10,Num 7]
        t = TInt 
        initf = (.* Num 3)
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






