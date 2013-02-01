module Example where

import Push

ex1 :: Program
ex1 = iff (Num 0) (Skip)
                 (Skip)
