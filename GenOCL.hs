module GenOCL where

import Util
import PIRE

{- 
 - My idea: Generate regular C, but offload Parallel loops to GPU via OpenCL interface
-}

-- TODO parameterize gen over Host and Kernel to avoid code duplication.
-- TODO Text.PrettyPrint

gen :: Program -> Gen ()
gen Skip = line "0;"

gen (Assign name es e) = line $ show (Index name es) ++ " = " ++ show e ++ ";"

gen (p1 :>> p2) = gen p1 >> gen p2

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

gen (Par start max p) = do
    line "// Par in host code"
--  d <- incVar
--  let i = ([ "i", "j", "k" ] ++ [ "i" ++ show i | i <- [0..] ]) !! d
--  let kerName = 'k' : show d
--  lineK $ "__kernel void " ++ kerName ++ " ( __global int *A, __global int *res) {"
--  lineK "int tid = get_global_id(0);"
--  lineK "if( tid < max ) {"
--  gen (p (var i))
--
--  -- assume Parameters A,res
--  lineK $ "res [tid] = " ++ "A[tid];"
--
--  lineK "}"
--
--  lineK "}"

gen (For e1 e2 p) = do
  d <- incVar
  let i = ([ "i", "j", "k" ] ++ [ "i" ++ show i | i <- [0..] ]) !! d
  line $ show TInt ++ " " ++ i ++ ";"
  line $ "for( " ++ i ++ " = " ++ show e1 ++ "; " 
                 ++ i ++ " < " ++ show e2 ++ "; " ++ i ++ "++ ) {"
  indent 2
  gen (p (var i))
  unindent 2
  line "}"

gen (Alloc siz f) = do 
  d <- incVar
  let m = "mem" ++ show d
  line $ m ++ " = malloc(" ++ show siz ++ ");"
  gen $ f (locArray m) (array m siz)
  line $ "free(" ++ m ++ ");"


{- TODO: Compile the program (f) into a kernel.
 - Return kernel information s.t. we can pass/read
 - memory to/from GPU.
 - Program should probably not be parameterized over Expr,
 - but rather over the tid (i.e. not parameterized at all).

 - Pseudo:
 (AllocNew t siz f) = do
  kernInfo <- compileToKernel f -- output f to kernel.
  gen kernInfo                  -- make host program generate whatever needs to be generated (cl_mems,
                                   -- enqueueWriteBuffer, createProgramWithSource, buildProgram etc..)
-}

gen (AllocNew t siz f) = do
  d <- getVar
  let m = "mem" ++ show d
  line $ show t ++ " " ++ m ++ " = malloc(" ++ "sizeof(" ++ show t ++ ")*" ++ show siz ++ ");"
  k <- genKernel f -- f :: (Expr -> Program) -> Program. k is info about kernel
  line "// do memory allocation for OCL"
  return ()

data Kernel = Kernel --Placeholder

genKernel :: (Loc Expr -> Program) -> Gen Kernel
genKernel f = genKernel' (f (locArray  "res" (var "tid")))  -- TODO This needs to be more controlled
              >> return Kernel
  where
    -- We need to treat Programs differently in the kernel code (I think?)
    genKernel' :: Program -> Gen ()
    genKernel' Skip = lineK "0;"
    genKernel' (Assign name es e) = lineK $ show (Index name es) ++ " = " ++ show e ++ ";"
    genKernel' (p1 :>> p2) = genKernel' p1 >> genKernel' p2
    genKernel' (If c p1 p2) = do
                lineK $ "if( " ++ show c ++ " ) { "
                indent 2
                genKernel' p1
                unindent 2
                lineK "else { "
                indent 2
                genKernel' p2
                unindent 2
                lineK "}"

    genKernel' (Par _ max p) = do
      d <- incVar
      let tid = "tid"
      let kerName = 'k' : show d
      lineK $ "__kernel void " ++ kerName ++ " (__global int *A, __global int *res) {"
      lineK "int tid = get_global_id(0);"
      lineK $ "if( tid < " ++ show max ++ " ) {"
      genKernel' $ p (var tid)

      lineK "}"
      lineK "}"

    genKernel' (For e1 e2 p) = do
                  d <- incVar
                  let i = ([ "i", "j", "k" ] ++ [ "i" ++ show i | i <- [0..] ]) !! d
                  lineK $ show TInt ++ " " ++ i ++ ";"
                  lineK $ "for( " ++ i ++ " = " ++ show e1 ++ "; " 
                                  ++ i ++ " < " ++ show e2 ++ "; " ++ i ++ "++ ) {"
                  indent 2
                  genKernel' (p (var i))
                  unindent 2
                  lineK "}"

    genKernel' (Alloc siz f) = do 
      d <- incVar
      let m = "mem" ++ show d
      lineK $ m ++ " = malloc(" ++ show siz ++ ");"
      genKernel' $ f (locArray m) (array m siz)
      lineK $ "free(" ++ m ++ ");"
    genKernel' (AllocNew _ _ _) = error "allocNew in genKernel'"

 
setupHeadings :: Gen ()
setupHeadings = do
  line "#include <stdio.h>"
  line "#include <stdlib.h>"
  line "#include <CL/cl.h>"
  line "#define MAX_SOURCE_SIZE (0x100000)\n\n"
  line "int main (void) {"
  indent 2

setupEnd :: Gen ()
setupEnd = line "return 0;" >> unindent 2 >> line "}"

setupOCL :: Gen ()
setupOCL = do
  let fp     = "fp"
      srcStr = "source_str"
      srcSize = "source_size"
  kernels <- getKernelFile

  line $ "FILE *" ++ fp ++ " = NULL;"
  line $ "char* " ++ srcStr ++ ";"
  line $ fp ++ " = fopen( \"" ++ kernels ++ "\" , \"r\");"
  line $ srcStr ++ " = (char*) malloc(MAX_SOURCE_SIZE);"
  line $ "size_t " ++ srcSize ++ " = fread( " ++ srcStr ++ ", " ++ "1, " ++
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
         ", " ++ deviceID ++ ", 0, NULL);"
  line "\n\n"



