{-# LANGUAGE TypeFamilies #-}
module Gen where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Data.List 

import Expr

showProg :: Gen () -> IO ()
showProg prog = putStr $ unlines $ pre' ++ initfunc ++ proch ++ host ++ post' ++ kern'
  where (_,w) = evalRWS prog () emptyEnv
        pre'  = pre w
        proch = procHead w
        post' = post w
        host  = hostCode w
        kern  = kernelFunctions ++ kernCode w
        kern' = if null kern then [] else ["\n//Kernel code"] ++ kern
        initfunc = ["void init() {\n"] ++ initBlock w ++ ["\n}"]



toFile :: FilePath -> Gen () -> IO ()
toFile path prog = do writeFile path $ pre' ++ initfunc ++ proch ++ host ++ post'
                      unless (null kern) $ writeFile kernPath kern
  where (_,s,w) = runRWS prog () emptyEnv
        pre'  = unlines $ pre w
        proch = unlines $ procHead w
        post' = unlines $ post w
        host  = unlines $ hostCode w
        kernPath = kernelFile s
        kern  = unlines $ kernelFunctions ++ kernCode w
        initfunc = unlines $ ["void init() {\n"] ++ initBlock w ++ ["\n}"]
--writeFile path (unlines $ extractCode prog emptyEnv) >>
--                   writeFile (kernelFile emptyEnv) (unlines $ extractCodeK prog emptyEnv)

kernelFunctions :: [String]
kernelFunctions = ["int testBit_fun_int32_t( int x, int i ) {\n" ++
                  "  return (x & 1 << i) != 0;\n" ++
                  "}"]



class GenCode a where
  gen :: a -> Gen ()

type Gen = RWS () Writers Env -- Reader is currently unused, hence Unit.

data Writers = Writers
             { hostCode  :: [String]
             , decls     :: [String]
             , kernCode  :: [String]
             , initBlock :: [String]
             , pre       :: [String]
             , procHead  :: [String]
             , post      :: [String] -- trailing "}" etc.
             }

instance Monoid Writers where
  mempty      = Writers mempty mempty mempty mempty mempty mempty mempty
  mappend a b =  Writers { hostCode = mappend (hostCode a) (hostCode b)
                         , decls    = mappend (decls a) (decls b)
                         , kernCode = mappend (kernCode a) (kernCode b)
                         , initBlock = mappend (initBlock a) (initBlock b)
                         , pre      = mappend (pre a) (pre b)
                         , procHead = mappend (procHead a) (procHead b)
                         , post     = mappend (post a) (post b)
                         }


data Env = Env { varCount      :: Int             -- Variable counter
               , iDepth        :: Int             -- Host code indent depth
               , kernelFile    :: FilePath        -- Name of the file containing kernels
               , kiDepth       :: Int             -- Kernel indent depth
               , kernelCounter :: Int             -- Number of kernels generated "so far"
               , kernelNames   :: [String]        -- Names of kernels used
               , usedVars      :: [String]        -- Which "memory" objects have already been declared (to avoid redecl)
               , params        :: [String]        -- Parameters for the Procedure head
               }


line :: String -> Gen ()
line s = do d <- gets iDepth
            let ind = concat $ replicate d " "
            tell $ mempty {hostCode = [ind ++ s]}

decl :: String -> Gen ()
decl s = tell $ mempty {procHead = ["  " ++ s]}

addParam :: String -> Gen ()
addParam s = modify $ \env -> env{params = params env ++ [s]}

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
newLoopVar :: Gen String
newLoopVar = do v <- incVar
                return $ (map concat 
                           (group (group ['i' .. 'z' ])) ++
                             [ 'i' : show i | i <- [0..] ]) !! v

nameExists :: Name -> Gen Bool
nameExists n = fmap (elem n) (gets usedVars)

addUsedVar :: Name -> Gen ()
addUsedVar n = modify $ \env -> env {usedVars = n : usedVars env}


getKernelFile :: Gen String
getKernelFile = gets kernelFile


lineK :: String -> Gen ()
lineK s = do d <- gets kiDepth
             let ind = concat $ replicate d " "
             tell $ mempty {kernCode = [ind ++ s]}


emptyEnv :: Env
emptyEnv = Env 0 0 "kernels.cl" 0 0 [] [] []
