module Util where

{- 
 - Utility functions for generating code.
-}

import Control.Monad.State



type Gen a = State Env a

data Env = Env 
          { varCount   :: Int      -- Variable counter
          , code       :: [String] -- Accumulated code
          , iDepth     :: Int      -- Indent depth
          , kernelFile :: FilePath -- Name of the file containing kernels
          , kernelCode :: [String] -- Accumulated kernel code
          }

extractCode :: Gen a -> Env -> [String]
extractCode g e = code $ execState g e

line :: String -> Gen ()
line s = modify $ \env -> env{code = code env ++ 
                                      lines
                                        (concat (replicate (iDepth env) " ") ++ s)


                             }

indent :: Int -> Gen ()
indent i = modify $ \env -> env{iDepth = iDepth env + i}


unindent :: Int -> Gen ()
unindent i = modify $ \env -> env{iDepth = iDepth env - i}

getVar :: Gen Int
getVar = gets varCount


incVar :: Gen Int
incVar = do
  d <- getVar
  modify $ \env -> env{varCount = varCount env + 1}
  return d

emptyEnv :: Env
emptyEnv = Env 0 [] 0 "kernels.cl" []


------------------------------------------------------------
-- Kernels

getKernelFile :: Gen String
getKernelFile = gets kernelFile


lineK :: String -> Gen ()
lineK s = modify $ \env -> env {kernelCode = kernelCode env ++ [s]}

extractCodeK :: Gen a -> Env -> [String]
extractCodeK g e = kernelCode $ execState g e
