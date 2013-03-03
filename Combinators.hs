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
initialize :: Type -> Size -> (Index -> Expr) -> (ArrayName -> Program a) -> Program a
initialize t s f prog = Alloc' t s $ \partialLoc arrayName -> 
                         for (Num 0) s (\e -> partialLoc e (f e)) -- Initialization loop
                        .>> 
                          prog arrayName                   -- Followed by the rest of the program


mapP :: Type -> Size -> (Expr -> Expr) -> Program a
mapP t siz f = Alloc' t siz $ \loc' _ -> for (Num 0) siz $ \e -> loc' e (f (Num 2))

test :: Program a
test = Alloc' t len $ \partialLoc arrName1 ->
                      for (Num 0) len (\e -> partialLoc e (f e e))
                      .>>
                        (Alloc' t len $ \loc' _ ->
                          for (Num 0) len $ \e -> loc' e (f arrName1 arrName1))

--initialize (TPointer TInt) len (.+ (Num 45)) $
       -- \named -> mapP (TPointer TInt) len (f named)
  where len = Num 10
        f = (.+)
        t = (TPointer TInt)



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






