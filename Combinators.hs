{-# LANGUAGE GADTs #-}

module Combinators where

-- A combinator library

import Program
import Procedure
import GenOCL
import Util
import Gen

import Types
import Expr

import Data.Monoid
import Control.Monad.RWS
-----------------------------------------------------------------------------
-- Interface

-- | Initialize an array of length s and type t with function f, followed by the remaining program prog.
--initArray :: Type -> Dim -> ([Index] -> Expr) -> (PartialLoc Expr a -> IndexedArray -> Program a) -> Program a
--initArray t dim f prog = Alloc t dim $ \partialLoc arrayName -> 
--                            nestFor dim partialLoc f []   -- Build a nesting of for-loops
--                          .>>
--                            prog partialLoc arrayName     -- Followed by the rest of the program
--
--initScalar :: Type -> Expr -> (PartialLoc Expr a -> IndexedArray -> Program a) -> Program a
--initScalar t = initArray t [Num 1] . const
--
--
---- | Prints an array arr of type t and size s.
--printArray :: Type -> Dim -> IndexedArray -> Program a
--printArray t dim arr = nestFor' dim (\is -> Print t $ arr is) []
----for (Num 0) s $ \e -> Print t $ arr [e]
--
--
--
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
---- | Experimental map (to have something to play around with).
--mapP :: Type -> Dim -> ([Index] -> Expr) -> IndexedArray -> (IndexedArray -> Program a) -> Program a
--mapP :: (Expr -> Expr) -> Size -> ((Name -> Program a) -> Program a) -> Program a
--mapP f sz k = k $ \name -> for (Num 0) sz $ \e -> locArray name e (f e)

--mapP t dim f arr prog = initArray t dim f $ \loc res ->
--                          nestPar dim loc (\xs -> f [arr $ reverse xs]) []
--                      .>> prog res
--
---- | sequential scanl on 1D array using f.
--scan :: Type -> Dim -> (Expr -> Expr -> Expr) -> IndexedArray -> (IndexedArray -> Program a) -> Program a
--scan t dim f arr prog = Alloc t [head dim .+ Num 1] $ \partialLoc iarr -> 
--                          for (Num 1) (head dim .+ Num 1) 
--                              (\e -> partialLoc [e] (f (iarr [e .- Num 1]) (arr [e .- Num 1])))
--                      .>> prog iarr
--
---- | Sequential fold on 1D array using f.
--fold :: Type -> Size -> (Expr -> Expr -> Expr) -> Expr -> IndexedArray -> (IndexedArray -> Program a) -> Program a
--fold t s f acc arr prog = initScalar t acc $ \loc res -> 
--                            for (Num 0) s (\e -> loc [Num 0] (f (res [Num 0]) (arr [e])))
--                        .>> prog res
--
---- | Parallel fold. Works only when length is 2^length.
--foldP :: Type -> Size -> (Expr -> Expr -> Expr) -> Expr -> IndexedArray -> (IndexedArray -> Program a) -> Program a
--foldP t s f acc arr prog = initArray t [s] (\_ -> acc) $ \loc res -> 
--                            par (Num 0) s (\_ -> for (Num 0) s 
--                              (\i -> seqIf (s ./ Num 2)                  -- No. of if's
--                                    s                                    -- starting value
--                                    (flip (.==) (Num 0) . (.%) i)        -- conditional function
--                                    $ \current -> if current == Num 2 then 
--                                      loc [i] (f (arr [i]) (arr [i .+ (current ./ Num 2)])) else
--                                      loc [i] (f (res [i]) (res [i .+ (current ./ Num 2)]))
--                                    ))
--                        .>> prog res
--
--zipWithS :: Type -> Dim -> (Expr -> Expr -> Expr) -> IndexedArray -> IndexedArray -> (IndexedArray -> Program a) -> Program a
--zipWithS t dim f x1 x2 prog = initArray t dim (const (Num 0)) $ \loc res ->
--                                nestFor dim loc (\is -> f (x1 is) (x2 is)) []
--                            .>> prog res
--                                  
--
---- TODO: nest par-loops
--zipWithP :: Type -> Dim -> (Expr -> Expr -> Expr) -> IndexedArray -> IndexedArray -> (IndexedArray -> Program a) -> Program a
--zipWithP t dim f x1 x2 prog = initArray t dim (const (Num 0)) $ \loc res ->
--                                par (Num 0) (head dim) (\e -> loc [e] (f (x1 [e]) (x2 [e])))
--                                --nestPar dim loc (\is -> f (x1 is) (x2 is)) []
--                            .>> prog res
--
--
--
--
-------------------------------------------------------------------------------
---- Example programs
--
---- | With initialize and mapP helper functions.
mapTest :: Program a
mapTest = BasicProc $ 
            InParam (TPointer TInt)  $ \arr1 -> 
            OutParam (TPointer (TPointer TInt)) $ \out ->
              --Alloc TInt [sz] $ \allocName -> 
                (par (Num 0) sz $ \e -> locArray out e (f $ Index Host arr1 [e]))
                .>> testFor
              --  .>> loc out (var allocName)
          where
            f = (.*) (Num 2)
            sz = Num 16
              -- mapP (.* (Num 2)) (Num 10) $ loc
                      
--mapTest = initArray t dim initf $
--            \_ arrName -> mapP t dim apply arrName $
--              \mappedArr -> printArray t dim mappedArr
--  where dim = [Num 10]
--        t = TInt 
--        initf xs = (Num 3 .+) $ foldr1 (.*) xs 
--        apply xs = (xs !! 0) .+ Num 5
--
--scanTest :: Program a
--scanTest = initArray t dim initf $
--              \_ arrName -> scan t dim apply arrName $
--                \scannedArr -> printArray t dim scannedArr
--  where dim = [Num 10]
--        t = TInt 
--        initf xs = (Num 3 .+) $ foldr1 (.*) xs 
--        apply e1 e2 = e1 .+ e2
--
--foldTest :: Program a
--foldTest = initArray t dim initf $
--              \_ arrName -> foldP t (head dim) apply acc arrName $
--                \foldedName -> printArray t [Num 1] foldedName
--  where dim = [Num 128]
--        acc = Num 0
--        t   = TInt 
--        initf xs = foldr1 (.*) xs --(Num 3 .+) $ foldr1 (.*) xs
--        apply = (.+)
--
--dotProd :: Program a
--dotProd = initArray t dim initf $
--            \_ arr1 -> initArray t dim initf $
--              \_ arr2 -> zipWithP t dim (.*) arr1 arr2 $ 
--                \zipRes -> foldP t (head dim) (.+) acc zipRes $
--                  \foldRes -> printArray t [Num 1] foldRes
--  where dim = [Num 512]
--        t   = TInt 
--        acc = Num 0
--        --initf xs = (Num 3 .+) $ foldr1 (.*) xs
--        initf xs = foldr1 (.+) xs
--
--zipWithTest :: Program a
--zipWithTest = initArray t dim initf $
--            \_ arr1 -> initArray t dim initf $
--              \_ arr2 -> zipWithP t dim (.*) arr1 arr2 $ 
--                \zipRes -> printArray t dim zipRes
--  where dim = [Num 10]
--        t = TInt 
--        initf xs = foldr1 (.*) xs
--
--exampleFold :: Gen ()
--exampleFold = setupHeadings >> setupOCL >> gen foldTest >> setupEnd
--
--exampleMap :: Gen ()
--exampleMap = setupHeadings >> setupOCL >> gen mapTest >> setupEnd
--
--exampleScan :: Gen ()
--exampleScan = setupHeadings >> setupOCL >> gen scanTest >> setupEnd
--
--exampleDotProd :: Gen ()
--exampleDotProd = setupHeadings >> setupOCL >> gen dotProd >> setupEnd
--
--exampleZipWith :: Gen ()
--exampleZipWith = setupHeadings >> setupOCL >> gen zipWithTest >> setupEnd
--

------------------------------------------------------------
-- helpers





--toFile :: Gen () -> FilePath -> IO ()
--toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
--                   writeFile (kernelFile emptyEnv) (unlines $ extractCodeK prog emptyEnv)






