module GenOCL where

import Util
import Push

{-
My idea: Generate regular C, but offload Parallel loops to GPU via OpenCL interface
-}

gen :: Program -> Gen ()
gen Skip = line "0;"

gen (Assign name es e) = line $ show (Index name es) ++ " = " ++ show e ++ ";"

gen (p1 :>> p2) = gen p1 >> gen p2

-- match out some more cases (Skip)
gen (If c p1 p2) = do
  line $ "if( " ++ show c ++ " ) { "
  indent 2
  gen p1
  unindent 2
  line "else { "
  indent 2
  gen p2
  unindent 2
  line "}"

-- This is where most openCl specific things go, I think.
gen (For e1 e2 p) = do
  d <- incVar
  let i = ([ "i", "j", "k" ] ++ [ "i" ++ show i | i <- [0..] ]) !! d
  line $ "for( " ++ i ++ " = " ++ show e1 ++ "; " 
                 ++ i ++ " < " ++ show e2 ++ "; " ++ i ++ "++ ) {"
  indent 2
  gen (p (var i)) -- this program is to be kernelized
  unindent 2
  line "}"

gen (Alloc siz f) = do
  d <- incVar
  let m = "mem" ++ show d
  line $ m ++ " = malloc(" ++ show siz ++ ");"
  gen $ f (locArray m) (array m siz)
  line $ "free(" ++ m ++ ");"


ex1 :: Program
ex1 = Alloc (Num 5) (\f arr -> 
                        If (Num 1) (Assign "x" [] (Num 5))
                           (For (Num 0) (Num 10) (\i -> f (var "y") (Num 5)))
                    )
