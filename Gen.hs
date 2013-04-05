{-# LANGUAGE TypeFamilies #-}
module Gen where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
--import qualified Data.Map as Map
import Data.List 

import Expr

class GenCode a where
  gen :: a -> Gen ()

type Gen = RWS () Writers Env




data Writers = Writers
             { hostCode   :: [String]
             , kernCode :: [String]
             }

instance Monoid Writers where
  mempty      = Writers mempty mempty
  mappend a b =  Writers { hostCode = mappend (hostCode a) (hostCode b)
                         , kernCode = mappend (kernCode a) (kernCode b)
                         }


data Env = Env { varCount      :: Int             -- Variable counter
               , iDepth        :: Int             -- Indent depth
               , kernelFile    :: FilePath        -- Name of the file containing kernels
               , kiDepth       :: Int             -- Kernel indent depth
               , kernelCounter :: Int             -- Number of kernels generated "so far"
               , usedVars      :: [String]
               }


line :: String -> Gen ()
line s = tell $ mempty {hostCode = [s]} --modify $ \env -> env{code = code env ++ 
                   --                   lines
                   --                     (concat (replicate (iDepth env) " ") ++ s)}


extractCode :: Gen a -> Env -> [String]
extractCode g env = let (_,w) = evalRWS g () env in hostCode w

extractCodeK :: Gen a -> Env -> [String]
extractCodeK g env = let (_,_,w) = runRWS g () env
                     in kernCode w

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
lineK s = tell $ mempty {kernCode = [s]}
-- modify $ \env -> env {kernelCode = kernelCode env ++ 
--                                       lines 
--                                         (concat (replicate (kiDepth env) " " ) ++ s)}



emptyEnv :: Env
emptyEnv = Env 0 0 "kernels.cl" 0 0 []
