module Util where

{- 
 - Utility functions for generating code.
 - Attempts to be as language independent as possible.
 -}

import Control.Monad.State

type Gen a = State Env a

data Env = Env 
          { varCount   :: Int    -- variable counter
          , code       :: [String] -- Accumulated code
          , iDepth     :: Int    -- Indent depth
          , kernelFile :: FilePath -- name of the file containing kernels
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

getKernelFile :: Gen String
getKernelFile = gets kernelFile

incVar :: Gen Int
incVar = do
  d <- getVar
  modify $ \env -> env{varCount = varCount env + 1}
  return d

emptyEnv :: Env
emptyEnv = Env 0 [] 0 "kernels.cl"


------------------------------------------------------------
-- OpenCL & related

setupHeadings :: Gen ()
setupHeadings = do
  line "include <stdio.h>"
  line "include <stdlib.h>"
  line "include <CL/cl.h>"
  line "#define MAX_SOURCE_SIZE (0x100000)\n\n"
  line "main (void) {"
  indent 2

setupEnd :: Gen ()
setupEnd = unindent 2 >> line "}"

setupOCL :: Gen ()
setupOCL = do
  let fp     = "fp"
      srcStr = "source_str"
      srcSize = "source_size"
  kernels <- getKernelFile

  line $ "FILE *" ++ fp ++ " = NULL;"
  line $ srcStr ++ ";"
  line $ fp ++ " = fopen( \"" ++ kernels ++ "\" , \"r\");"
  line $ srcStr ++ " = (char*) malloc(MAX_SOURCE_SIZE);"
  line $ srcSize ++ " = fread( " ++ srcStr ++ ", " ++ "1, " ++
                    "MAX_SOURCE_SIZE, " ++ fp ++ ");"
  line $ "fclose( " ++ fp ++ " );"
  
  let platformID = "platform_id"
      deviceID   = "device_id"
      numDevices = "ret_num_devices"
      numPlatforms = "ret_num_platforms"
      context      = "context"
      queue        = "command_queue"
      
  line $ "cl_platform_id " ++ platformID ++ " = NULL;"
  line $ "cl_device_id " ++ deviceID ++ " = NULL;"
  line $ "cl_uint " ++ numDevices ++ ";"
  line $ "cl_uint " ++ numPlatforms ++ ";"
  line $ "clGetPlatformIDs(1, &" ++ platformID ++ ", &" ++ numPlatforms ++ ");"
  line $ "clGetDeviceIDs(" ++ platformID ++ ", CL_DEVICE_TYPE_DEFAULT, 1, " ++
         "&" ++ deviceID ++ ", &" ++ numDevices ++ ");"
  line $ "cl_context " ++ context ++ " = clCreateContext(NULL, 1, &" ++ deviceID ++ ", NULL, NULL, NULL);"
  line $ "cl_command_queue " ++ queue ++ " = clCreateCommandQueue(" ++ context ++ 
         ", " ++ deviceID ++ ", " ++ ", 0, NULL);"


example :: IO ()
example = putStr $ unlines $ extractCode (setupHeadings >> setupOCL >> setupEnd) emptyEnv
