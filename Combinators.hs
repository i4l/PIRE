module Combinators where

-- A small combinator library

import PIRE
import GenOCL
import Util

--singleton :: Array Pull Expr
--singleton = array "a" (Num 1)

add, mul :: Int -> Int -> Expr

add x y = Num x .+ Num y
mul x y = Num x .* Num y

(??) :: Bool -> (Program, Program) -> Program
True  ?? (p1,p2)  = iff (Num 1) p1 p2
False ?? (p1,p2)  = iff (Num 0) p1 p2


-- Parallelise this via openCl
parProg :: Int -> (Expr -> Expr) -> Program
parProg len f = AllocNew (TPointer TInt) (Num len) $ \location arr -> par (Num 0) (Num len) 
                                                                     (\e -> location  (f 
                                                                                       (pull (doit arr) e) 
                                                                                      )
                                                                     )
                                                                 -- location :: Loc Expr :: Expr -> Program
                                                                 -- lambda   :: (Expr -> Program) -> Program

forProg :: Int -> (Expr -> Expr) -> Program
forProg len f = Alloc  (Num len) $
  \allocf arr -> for (Num 0) (Num len)
                 (\e -> allocf e 
                        (f 
                         (pull (doit arr) (Num 5)) 
                        ) 
                 )

fe :: Expr -> Expr
fe e = e .+ e

exPar :: Program
exPar = parProg 10 fe

exFor :: Program
exFor = forProg 10 fe

example :: Gen ()
--example = setupHeadings >> setupOCL >> gen exPar >> setupEnd
example = setupHeadings >> gen exPar2 >> setupEnd

-- TODO don't want to mention explicit array names
exPar2 :: Program
exPar2 = parProg 10 $ fe

------------------------------------------------------------
-- helpers

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ (extractCode prog emptyEnv) ++ (extractCodeK prog emptyEnv)


toFile :: Gen () -> FilePath -> IO ()
toFile prog path = writeFile path (unlines $ extractCode prog emptyEnv)

