{-# LANGUAGE GADTs #-}

module Combinators where

-- A combinator library

import PIRE
import GenOCL
import Util
import Gen

import Types
import Expr


-----------------------------------------------------------------------------
-- Interface

-- | Initialize an array of length s and type t with function f, followed by the remaining program prog.
initialize :: Type -> Size -> (Index -> Expr) -> (IndexedArray -> Program a) -> Program a
initialize t s f prog = Alloc' t s $ \partialLoc arrayName -> 
                         for (Num 0) s (\e -> partialLoc [e] (f e)) -- Initialization loop
                        .>> 
                          prog arrayName                          -- Followed by the rest of the program


initialize2 :: Type -> Size -> (Index -> Expr) -> (IndexedArray -> Program a) -> Program a
initialize2 t s f prog = Alloc' (TPointer t) (s.*s) $ \partialloc arrName -> 
                            for (Num 0) s $ \e -> 
                              for (Num 0) s (\e' -> partialloc [e,e'] (f e'))

                        .>> 
                          prog arrName
                        

mapP :: Type -> Size -> IndexedArray -> (Expr -> Expr) -> Program a
mapP t siz arr f = Alloc' t siz $ \loc' _ -> for (Num 0) siz $ \e -> loc' [e] (f $ arr [e])


init2D :: Program a
init2D = initialize2 t len initf $ \arrName -> mapP t len arrName apply
  where len = Num 10
        t = TPointer TInt
        initf = (.* Num 3)
        apply = (.+ Num 5)




-----------------------------------------------------------------------------
-- Example programs

-- | With initialize and mapP helper functions.
mapTest :: Program a
mapTest = initialize t len initf $
         \arrName -> mapP t len arrName apply
  where len = Num 10
        t = TPointer TInt
        initf = (.* Num 3)
        apply = (.+ Num 5)


-- | Without initialize and mapP
mapTest2 :: Program a
mapTest2 = Alloc' t len $ \partialLoc arrName1 ->
                      for (Num 0) len (\e -> partialLoc [e] (initf e))
                      .>>
                        Alloc' t len $ \loc' _ ->
                          for (Num 0) len $ \e -> loc' [e] (apply $ arrName1 [e])
  where len = Num 10
        t = TPointer TInt
        initf = (.* Num 3)
        apply = (.+ Num 5)


--example :: Gen ()
--example = setupHeadings >> setupOCL >> gen vecMul >> setupPrint "mem1" 10 >> setupEnd

------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ (extractCode prog emptyEnv) ++ (extractCodeK prog emptyEnv)


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv) >>
                   writeFile (kernelFile emptyEnv) (unlines $ extractKernelCode prog emptyEnv)






