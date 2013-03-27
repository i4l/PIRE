module Gen where

import Control.Monad.State
import qualified Data.Map as Map
import Data.List 

import Expr


type Gen a = State Env a

data Env = Env 
          { varCount      :: Int             -- Variable counter
          , code          :: [String]        -- Accumulated code
          , iDepth        :: Int             -- Indent depth
          , kernelFile    :: FilePath        -- Name of the file containing kernels
          , kernelCode    :: [String]        -- Accumulated kernel code
          , kiDepth       :: Int             -- Kernel indent depth
          , kernelCounter :: Int            -- Number of kernels generated "so far"
          , usedVars      :: [String]
         -- , paramCounter :: Int             -- Kernel parameter counter
         -- , paramMap     :: Map.Map Int Int -- Mapping AllocID -> Kernel params.
         -- , hostAllocMap :: Map.Map Int Int -- Mapping Kernel Params -> AllocID
         -- , inits        :: Map.Map Int (Index -> Expr) -- AllocID -> ixf 
          }

line :: String -> Gen ()
line s = modify $ \env -> env{code = code env ++ 
                                      lines
                                        (concat (replicate (iDepth env) " ") ++ s)}


extractCode :: Gen a -> Env -> [String]
extractCode g e = code $ execState g e

extractKernelCode :: Gen a -> Env -> [String]
extractKernelCode g e = kernelCode $ execState g e

indent :: Int -> Gen ()
indent i = modify $ \env -> env{iDepth = iDepth env + i}

unindent :: Int -> Gen ()
unindent i = modify $ \env -> env{iDepth = iDepth env - i}

-- kernel indent
kindent :: Int -> Gen ()
kindent i = modify $ \env -> env{kiDepth = kiDepth env + i}

kunindent :: Int -> Gen ()
kunindent i = modify $ \env -> env{kiDepth = kiDepth env - i}


-- | Generate a fresh name and increase the counter.
incVar :: Gen Int
incVar = do
  d <- gets varCount
  modify $ \env -> env{varCount = varCount env + 1}
  return d

-- | Generate a new loop variable (based on the variable counter from incVar).
newLoopVar :: Gen (String,Int)
newLoopVar = do v <- incVar
                return ((map concat 
                            (group (group ['i' .. 'z' ])) ++
                              [ 'i' : show i | i <- [0..] ]) !! v, v)

nameExists :: Name -> Gen Bool
nameExists n = fmap (elem n) (gets usedVars)

addUsedVar :: Name -> Gen ()
addUsedVar n = modify $ \env -> env {usedVars = n : usedVars env}


getKernelFile :: Gen String
getKernelFile = gets kernelFile


lineK :: String -> Gen ()
lineK s = modify $ \env -> env {kernelCode = kernelCode env ++ 
                                                          lines 
                                                            (concat (replicate (kiDepth env) " " ) ++ s)}


extractCodeK :: Gen a -> Env -> [String]
extractCodeK g e = kernelCode $ execState g e




emptyEnv :: Env
emptyEnv = Env 0 [] 0 "kernels.cl" [] 0 0 []
