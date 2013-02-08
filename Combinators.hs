module Combinators where

-- A small combinator library

import PIRE
import GenOCL
import Util


parProg :: Int -> (Expr -> Expr) -> Program
parProg len f = AllocNew (TPointer TInt) (Num len) $ \location arr -> par (Num 0) (Num len) 
                                                                      (\e -> location  (f 
                                                                                          (pull (doit arr) e) 
                                                                                       )
                                                                      )

dualPar :: (Expr -> Expr -> Expr) -> Int -> Program
dualPar f len = AllocNew (TPointer TInt) (Num len) $ \loc1 arr1 -> 
                  AllocNew (TPointer TInt) (Num len) $ \_ arr2 -> 
                    par (Num 0) (Num len) $ 
                      \e -> loc1 (f (pull (doit arr1) e) (pull (doit arr2) e))

vecMul :: Program
vecMul = dualPar (\a b -> a .* b) 10


-- A sequential for-loop for inspiration.
forProg :: Int -> (Expr -> Expr) -> Program
forProg len f = Alloc  (Num len) $
  \allocf arr -> for (Num 0) (Num len)
                 (\e -> allocf e 
                        (f 
                          (pull (doit arr) (Num 5)) 
                        ) 
                 )

add :: Expr -> Expr
add e = e .+ e

exPar :: Program
exPar = parProg 10 add

exFor :: Program
exFor = forProg 10 add

example :: Gen ()
example = setupHeadings >> gen vecMul >> setupEnd

exPar2 :: Program
exPar2 = parProg 10 $ add

------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ (extractCode prog emptyEnv) ++ (extractCodeK prog emptyEnv)


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv)

