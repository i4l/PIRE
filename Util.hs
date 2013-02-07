module Util where

{- 
 - Utility functions for generating code.
-}

import Control.Monad.State
import qualified Data.Map as Map



type Gen a = State Env a

data Env = Env 
          { varCount     :: Int             -- Variable counter
          , code         :: [String]        -- Accumulated code
          , iDepth       :: Int             -- Indent depth
          , kernelFile   :: FilePath        -- Name of the file containing kernels
          , kernelCode   :: [String]        -- Accumulated kernel code
          , paramCounter :: Int             -- Kernel parameter counter
          , paramMap     :: Map.Map Int Int -- Mapping allocations in Host -> Kernel params.
          }

extractCode :: Gen a -> Env -> [String]
extractCode g e = code $ execState g e

line :: String -> Gen ()
line s = modify $ \env -> env{code = code env ++ 
                                      lines
                                        (concat (replicate (iDepth env) " ") ++ s)}



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

getParamCounter :: Gen Int
getParamCounter = do
  p <- gets paramCounter
  return p

incParamCounter :: Gen Int
incParamCounter = do
  d <- getParamCounter
  modify $ \env -> env{paramCounter = paramCounter env + 1}
  return d


getParamMap :: Gen (Map.Map Int Int)
getParamMap = gets paramMap

addKernelParam :: Int -> Gen Int
addKernelParam hostAllocId = do
  new <- incParamCounter
  modify $ \env -> env {paramMap =  Map.insert hostAllocId new (paramMap env)}
  return new

lookupKernelParam :: Int -> Gen (Maybe Int)
lookupKernelParam hostAllocId = do
  m <- gets paramMap
  return $ Map.lookup hostAllocId m


printMap :: Gen a -> IO ()
printMap g = do
  let e = execState g emptyEnv
      m = Map.toList (paramMap e)
      m' = map (\(h,k) -> "mem" ++ show h ++ " is mapped to arr" ++  show k ++ "\n") m
  putStrLn $ concat m'


emptyEnv :: Env
emptyEnv = Env 0 [] 0 "kernels.cl" [] 0 Map.empty


------------------------------------------------------------
-- Kernels

getKernelFile :: Gen String
getKernelFile = gets kernelFile


lineK :: String -> Gen ()
lineK s = modify $ \env -> env {kernelCode = kernelCode env ++ [s]}

extractCodeK :: Gen a -> Env -> [String]
extractCodeK g e = kernelCode $ execState g e
