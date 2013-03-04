module Gen where

import Control.Monad.State
import qualified Data.Map as Map

import Expr

type Gen a = State Env a

data Env = Env 
          { varCount     :: Int             -- Variable counter
          , code         :: [String]        -- Accumulated code
          , iDepth       :: Int             -- Indent depth
          , kernelFile   :: FilePath        -- Name of the file containing kernels
          , kernelCode   :: [String]        -- Accumulated kernel code
          , paramCounter :: Int             -- Kernel parameter counter
          , paramMap     :: Map.Map Int Int -- Mapping AllocID -> Kernel params.
          , hostAllocMap :: Map.Map Int Int -- Mapping Kernel Params -> AllocID
          , inits        :: Map.Map Int (Index -> Expr) -- AllocID -> ixf 
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

-- | Generate a fresh name and increase the counter.
incVar :: Gen Int
incVar = do
  d <- gets varCount
  modify $ \env -> env{varCount = varCount env + 1}
  return d

-- | Generate a new loop variable (based on the variable counter from incVar).
newLoopVar :: Gen (String,Int)
newLoopVar = do
  v <- incVar
  return $ (([ "i", "j", "k" ] ++ [ "i" ++ show i | i <- [0..] ]) !! v, v)

getParamCounter :: Gen Int
getParamCounter = gets paramCounter

incParamCounter :: Gen Int
incParamCounter = do
  d <- getParamCounter
  modify $ \env -> env{paramCounter = paramCounter env + 1}
  return d

addKernelParam :: Int -> Gen Int
addKernelParam hostAllocId = do
  new <- incParamCounter
  modify $ \env -> env {paramMap =  Map.insert hostAllocId new (paramMap env)}
  modify $ \env -> env {hostAllocMap =  Map.insert new hostAllocId (hostAllocMap env)}
  
  return new

lookupForKernel :: Int -> Gen (Maybe Int)
lookupForKernel hostAllocId = do
  m <- gets paramMap
  return $ Map.lookup hostAllocId m

lookupForHost :: Int -> Gen (Maybe Int)
lookupForHost kernParam = do
  m <- gets hostAllocMap
  return $ Map.lookup kernParam m


printMap :: Gen a -> IO ()
printMap g = do
  let e = execState g emptyEnv
      m = Map.toList (paramMap e)
      m' = map (\(h,k) -> "mem" ++ show h ++ " is mapped to arr" ++  show k ++ "\n") m
  putStrLn $ concat m'
  let m2 = Map.toList (hostAllocMap e)
      m2' = map (\(k,h) -> "arr" ++ show k ++ " is mapped to mem" ++  show h ++ "\n") m2
  putStrLn $ concat m2'


addInitFunc :: Int -> (Index -> Expr) -> Gen ()
addInitFunc allocID f = modify $ \env -> env {inits = Map.insert allocID f (inits env)}

getInitFuncs :: Gen (Map.Map Int (Index -> Expr))
getInitFuncs = gets inits

emptyEnv :: Env
emptyEnv = Env 0 [] 0 "kernels.cl" [] 0 Map.empty Map.empty Map.empty
