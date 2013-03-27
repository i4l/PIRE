module GenOCL where

import Util
import Program
import Types
import Expr
import Gen
import Analysis

--import qualified Data.Map as Map
--import Data.Maybe
import Control.Monad.State
import Control.Monad
import Data.List
import Control.Applicative

gen :: Program a -> Gen ()

gen Skip = line ""

gen (Print t e) = do let printTerm = case t of
                                      TInt       -> "i"
                                      TPointer x -> error "ERROR: Attempt to use pointer in in printf."
                                      x@_        -> error "ERROR: Attempt to use unsupported type " ++ 
                                                           show x ++ "in printf."
                     line $ "printf(\"%" ++ printTerm ++ " \"" ++ ", " ++ show e ++ ");"

gen (Assign name es e) = line $ show (Index name es) 
                         ++ " = " ++ show e ++ ";"

gen (Statement e) = line $ show e ++ ";"

gen (p1 :>> p2) = gen p1 >> gen p2

gen (If c p1 Skip) = do line $ "if( " ++ show c ++ " )"
                        indent 2
                        gen p1
                        unindent 2
gen (If c p1 p2) = do line $ "if( " ++ show c ++ " ) { "
                      indent 2
                      gen p1
                      unindent 2
                      line "else { "
                      indent 2
                      gen p2
                      unindent 2
                      line "}"
gen (Par start end f) = do let tid = "tid"
                               paramTriples = grabKernelParams (f $ var tid)
                               parameters = (init . concat) [ " __global " ++ show t ++ " " ++  n ++ "," | (n,dim,t) <- paramTriples]
                           
                           --debugging code. prints the parameter names gathered.
                           --line "//Param triples"
                           --mapM_ line $ map ((++) "// " . show) (paramTriples)

                           kerName <- fmap ((++) "k" . show) incVar
                           lineK $ "__kernel void " ++ kerName ++ "(" ++ parameters ++ " ) {"
                           kindent 2
                           lineK $ show TInt ++ " " ++  tid ++ " = " ++ "get_global_id(0)" ++ ";"
                           lineK $ "if( tid < " ++ show end ++ " ) {"
                           kindent 2

                           let translated = parForUnwind (f $ var tid) tid
                           kindent 2
                           genK $ translated
                           kunindent 2

                           runOCL kerName
                           setupOCLMemory paramTriples 0 end
                           launchKernel 2048 1024
                           modify $ \env -> env {kernelCounter = kernelCounter env + 1}
                           let (n,dim,t) = head paramTriples
                           readOCL n (TPointer t) end
                           lineK "}"
                           kunindent 2
                           lineK "}"
                           kunindent 2
                           return ()

gen (For e1 e2 p) = do i <- fmap fst newLoopVar
                       line $ show TInt ++ " " ++ i ++ ";"
                       line $ "for( " ++ i ++ " = " ++ show e1 ++ "; " 
                           ++ i ++ " < " ++ show e2 ++ "; "
                           ++ i ++ "++ ) {"
                       indent 2
                       gen $ p (var i)
                       unindent 2
                       line "}"

gen (Alloc t dim f) = do d <- incVar
                         let m = "mem" ++ show d
                         nestForAlloc dim m t
                         gen  $ f (locNest m) (Index m)
                         line $ "free(" ++ m ++ ");\n"
 

-- Code gen in kernel code   
genK :: Program a -> Gen ()
genK (Print t e) = do let printTerm = case t of
                                      TInt       -> "i"
                                      TPointer x -> error "ERROR: Attempt to use pointer in in printf."
                                      x@_        -> error "ERROR: Attempt to use unsupported type " ++ 
                                                           show x ++ "in printf."
                      lineK $ "printf(\"%" ++ printTerm ++ " \"" ++ ", " ++ show e ++ ");"
genK Skip            = return ()
genK (Assign name es e) = do lineK (show (Index name es) ++ " = " ++ show e ++ ";")
genK (p1 :>> p2)        = genK p1 >> genK p2
genK (If c p1 Skip) = do lineK $ "if( " ++ show c ++ " )"
                         kindent 2
                         genK p1
                         kunindent 2
genK (If c p1 p2) = do lineK $ "if( " ++ show c ++ " ) { "
                       kindent 2
                       genK p1
                       kunindent 2
                       lineK "else { "
                       kindent 2
                       genK p2
                       kunindent 2
                       lineK "}"
genK (For e1 e2 p) = do i <- fmap fst newLoopVar
                        lineK $ show TInt ++ " " ++ i ++ ";"
                        lineK $ "for( " ++ i ++ " = " ++ show e1 ++ "; " 
                            ++ i ++ " < " ++ show e2 ++ "; "
                            ++ i ++ "++ ) {"
                        kindent 2
                        genK $ p (var i)
                        kunindent 2
                        lineK "}"
genK (Par start end f) = genK (For start end f)
genK (Alloc t dim f) = do argName <- fmap ((++) "mem" . show) incVar
                          lineK $ "// Alloc in Kernel"
                          genK $ f (locNest argName) (Index argName)
                          return ()





setupOCLMemory :: [(Name,Dim,Type)] -> Int -> Size -> Gen ()
setupOCLMemory []           i s = return ()
setupOCLMemory ((n,d,t):xs) i s = do nameUsed <- nameExists n -- If a name is already declared we can reuse it
                                     let objPostfix = "_obj"
                                         createBuffers = (if not nameUsed then "cl_mem " else "") ++ n ++ objPostfix ++ " = clCreateBuffer(context, " ++ 
                                                  "CL_MEM_READ_WRITE" ++ ", " ++ show s ++ "*sizeof(" ++ 
                                                  removePointer t ++ "), NULL, NULL);"
                                     line createBuffers
                                     let copyBuffers = "clEnqueueWriteBuffer(command_queue, " ++ n ++ 
                                                             objPostfix ++ ", CL_TRUE, 0, " ++ show s ++ "*sizeof(" ++ 
                                                             removePointer t ++"), " ++ n ++ ", 0, NULL, NULL);"

                                     -- set arguments to kernel
                                     let setArgs = "clSetKernelArg(kernel, " ++ show i ++ 
                                                         ", sizeof(cl_mem), (void *)&" ++ n ++ objPostfix ++ ");"
                                     line setArgs
                                     addUsedVar n
                                     when (i /= 0) (line copyBuffers) -- We don't copy the result array
                                     setupOCLMemory xs (i+1) s

runOCL :: Name -> Gen ()
runOCL kname = do --create kernel & build program
            kcount <- gets kernelCounter -- we can reuse declared openCL objects
            line $ (if kcount <= 0 then "cl_program " else "") ++ "program = clCreateProgramWithSource(context, 1, (const char **)&source_str, " ++
                   "(const size_t *)&source_size, NULL);"
            line "clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);"
            line $ (if kcount <= 0 then "cl_kernel " else "") ++ "kernel = clCreateKernel(program, \"" ++ kname ++ "\", NULL);" 

launchKernel :: Int -> Int -> Gen ()
launchKernel global local = do
  kcount <- gets kernelCounter
  line $ (if kcount <= 0 then "size_t " else "") ++ "global_item_size = " ++ show global ++ ";"
  line $ (if kcount <= 0 then "size_t " else "") ++ "local_item_size = "  ++ show local ++ ";"
  line "clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);"

-- reads argument 0 from kernel
readOCL :: Name -> Type -> Size -> Gen () 
readOCL n t s = do
  line $ "clEnqueueReadBuffer(command_queue, " ++ n ++ "_obj" ++ ", CL_TRUE, 0, " ++
          show s ++ "* sizeof(" ++ removePointer t ++ "), " ++ n ++ ", 0, NULL, NULL);\n\n"



------------------------------------------------------------
-- Extras

setupHeadings :: Gen ()
setupHeadings = do line "#include <stdio.h>"
                   line "#include <stdlib.h>"
                   kcount <- gets kernelCounter
                   line "#include <CL/cl.h>"
                   line "#define MAX_SOURCE_SIZE (0x100000)\n\n"
                   line "int main (void) {"
                   indent 2

setupEnd :: Gen ()
setupEnd = line "return 0;" >> unindent 2 >> line "}"

setupOCL :: Gen ()
setupOCL = do let fp     = "fp"
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
              
              let platformID   = "platform_id"
                  deviceID     = "device_id"
                  numDevices   = "ret_num_devices"
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


-- TODO FIXME I'm broken and outdated. 
setupPrint :: String -> Int -> Gen ()
setupPrint alloc len = do loopVar <- fmap fst newLoopVar
                          line $ "int " ++ loopVar ++ ";"
                          line $ "for (" ++ loopVar ++ " = 0; " 
                              ++ loopVar ++ " < " ++ show len ++ "; "
                              ++ loopVar ++ "++ ) {"
                          line $ "printf(\"%d\\n\"," ++ alloc 
                              ++ "[" ++ loopVar ++ "] );"
                          line "}"
