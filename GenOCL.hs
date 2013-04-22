{-# LANGUAGE StandaloneDeriving #-}

module GenOCL where

import Util
import Program
import Procedure
import Types
import Expr
import Gen
import Analysis

--import Control.Monad.State
import Control.Monad.RWS
import Data.List

-----------------------------------------------------------------------------
-- Show Instances

instance Show (Program a) where
  show p = unlines $ hostCode w
    where (_,w) = evalRWS (gen p) () emptyEnv

--deriving instance Show (Proc a)

-----------------------------------------------------------------------------
--instance GenCode (Proc a) where
--  gen = genProc
--
--genProc :: Proc a -> Gen ()
--genProc NilProc          = return ()
--genProc (BasicProc proc) = do i <- incVar
--                              gen proc
--                              ps <- fmap (intercalate ", " . filter (not . null)) (gets params)
--                              tell $ mempty {pre = ["void " ++ "f" ++ show i ++ "(" ++ ps ++ ") {"]}
--                              tell $ mempty {post = ["}"]}
--genProc (ProcBody p)   = gen p
--genProc (OutParam t p) = do i <- incVar
--                            addParam $ show t ++ " out" ++ show i
--                            --addParam $ sizeParam t $ "outC" ++ show i
--                            gen $ p ("out" ++ show i)
--genProc (NewParam t p) = do i <- incVar
--                            addParam $ show t ++ " arg" ++ show i
--                            --addParam $ sizeParam t $ "argC" ++ show i
--                            gen $ p ("arg" ++ show i)

-- | adds a size parameter for a an input or output parameter.
--sizeParam :: Type -> Name -> String
--sizeParam TInt         _ = ""
--sizeParam (TPointer t) n = show t ++ " " ++ n
--sizeParam (TArray   t) n = sizeParam (TPointer t) n

-----------------------------------------------------------------------------

instance GenCode (Program a) where
  gen = genProg

genProg :: Program a -> Gen ()

genProg (BasicProc p) = do 
                           i <- incVar
                           indent 2
                           setupHeadings
                           when (isParallel p) setupOCL
                           gen $ removeDupBasicProg p
                           unindent 2
                           ps <- fmap (intercalate ", " . filter (not . null)) (gets params)
                           tell $ mempty {pre = ["void " ++ "f" ++ show i ++ "(" ++ ps ++ ") {"]}
                           tell $ mempty {post = ["}"]}
genProg (OutParam t p) = do i <- incVar
                            addParam $ show t ++ " out" ++ show i
                            --addParam $ show TInt ++ " out" ++ show i ++ "c"
                            gen $ p ("out" ++ show i)
genProg (InParam t p) = do i <- incVar
                           addParam $ show t ++ " arg" ++ show i
                           addParam $ show TInt ++ " arg" ++ show i ++ "c"
                           gen $ p ("arg" ++ show i)


genProg Skip = line ""

genProg (Print t e) = do let printTerm = case t of
                                TInt       -> "i"
                                TPointer x -> error "ERROR: Attempt to use pointer in in printf."
                                x@_        -> error "ERROR: Attempt to use unsupported type " ++ 
                                                           show x ++ "in printf."
                         line $ "printf(\"%" ++ printTerm ++ " \"" ++ ", " ++ show e ++ ");"

genProg (Assign name es e) = line $ show name --(Index name es) 
                         ++ " = " ++ show e ++ ";"

genProg (Statement e) = line $ show e ++ ";"

genProg (p1 :>> p2) = gen p1 >> gen p2

genProg (If c p1 Skip) = do line $ "if( " ++ show c ++ " ) {"
                            indent 2
                            gen p1
                            unindent 2
                            line "}"
genProg (If c p1 p2) = do line $ "if( " ++ show c ++ " ) { "
                          indent 2
                          gen p1
                          unindent 2
                          line "else { "
                          indent 2
                          gen p2
                          unindent 2
                          line "}"
genProg (Par start end f) = do let tid = "tid"
                                   f' = iff (BinOp $ Expr.LT (var tid) end) (parForUnwind (f $ var tid) tid) Skip

                                   paramTriples = grabKernelParams f'
                                   parameters = (init . concat) 
                                      [ " __global " ++ show t ++ " " ++  n ++ "," 
                                        | (n,t) <- paramTriples]

                               kerName <- fmap ((++) "k" . show) incVar
                               lineK $ "__kernel void " ++ kerName ++ "(" ++ parameters ++ " ) {"
                               kindent 2

                               lineK $ show TInt ++ " " ++  tid ++ " = " ++ "get_global_id(0)" ++ ";"

                               genK f'

                               runOCL kerName
                               setupOCLMemory paramTriples 0 end
                               launchKernel 2048 1024
                               modify $ \env -> env {kernelCounter = kernelCounter env + 1}
                               readOCL (grabKernelReadBacks f') end

                               kunindent 2
                               lineK "}"

genProg (For e1 e2 p) = do i <- newLoopVar
                           line $ show TInt ++ " " ++ i ++ ";"
                           line $ "for( " ++ i ++ " = " ++ show e1 ++ "; " 
                               ++ i ++ " < " ++ show e2 ++ "; "
                               ++ i ++ "++ ) {"
                           indent 2
                           gen $ p (var i)
                           unindent 2
                           line "}"

genProg (Alloc t dim f) = do d <- incVar
                             let m = "mem" ++ show d
                             nestForAlloc dim m t
                             gen $ f m
                             line $ "free(" ++ m ++ ");\n"

genProg (Decl t f)     = do d <- incVar
                            let m = "mem" ++ show d
                            line $ show t ++ " " ++ m ++ ";"
                            gen $ f m

-- Code gen in kernel code   
genK :: Program a -> Gen ()
genK (Print t e) = do let printTerm = case t of
                                      TInt       -> "i"
                                      TPointer x -> error "ERROR: Attempt to use pointer in in printf."
                                      x@_        -> error "ERROR: Attempt to use unsupported type " ++ 
                                                           show x ++ "in printf."
                      lineK $ "printf(\"%" ++ printTerm ++ " \"" ++ ", " ++ show e ++ ");"
genK Skip            = return ()
genK (Assign name es e) = lineK  $ (show name) ++ " = " ++ show e ++ ";"
genK (p1 :>> p2)        = genK p1 >> genK p2
genK (If c p1 Skip) = do lineK $ "if( " ++ show c ++ " ) {"
                         kindent 2
                         genK p1
                         kunindent 2
                         lineK "}"
genK (If c p1 p2) = do lineK $ "if( " ++ show c ++ " ) { "
                       kindent 2
                       genK p1
                       kunindent 2
                       lineK "else { "
                       kindent 2
                       genK p2
                       kunindent 2
                       lineK "}"
genK (For e1 e2 p) = do i <- newLoopVar
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
                          genK $ f argName



-----------------------------------------------------------------------------
-- Other things that may need revising.

setupOCLMemory :: Parameters -> Int -> Size -> Gen ()
setupOCLMemory []           i s = return ()
setupOCLMemory ((n,t):xs) i sz = let s = sz
                                  in do nameUsed <- nameExists n -- If a name is already declared we can reuse it
                                        let objPostfix = "_obj"
                                            createBuffers = 
                                                     (if not nameUsed then "cl_mem " else "") ++ n ++ 
                                                     objPostfix ++ " = clCreateBuffer(context, " ++ 
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
                                        --when (i /= 0) (line copyBuffers) -- We don't copy the result array
                                        line copyBuffers
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

--readOCL :: Name -> Type -> Size -> Gen () 
readOCL :: Parameters -> Size -> Gen () 
readOCL []            _  = return ()
readOCL ((n,t):xs) sz = let s = sz
                        in do line $ "clEnqueueReadBuffer(command_queue, " ++ n ++ "_obj" ++ ", CL_TRUE, 0, " ++
                                show s ++ "*sizeof(" ++ removePointer t ++ "), *" ++ n ++ ", 0, NULL, NULL);\n\n"
                              readOCL xs sz
                                
--let s = case sz of
--                          Index a _ -> Index a [Num 0]
--                          a         -> a
--                 in line $ "clEnqueueReadBuffer(command_queue, " ++ n ++ "_obj" ++ ", CL_TRUE, 0, " ++
--           show s ++ "*sizeof(" ++ removePointer t ++ "), *" ++ n ++ ", 0, NULL, NULL);\n\n"



------------------------------------------------------------
-- Extras

setupHeadings :: Gen ()
setupHeadings = do tell $ mempty {pre = ["#include <stdio.h>"]}
                   tell $ mempty {pre = ["#include <stdlib.h>"]}
                   tell $ mempty {pre = ["#include <CL/cl.h>"]}
                   tell $ mempty {pre = ["#include \"feldspar_c99.h\""]}

                   tell $ mempty {pre = ["#define MAX_SOURCE_SIZE (0x100000)\n\n"]}
                   --line "int main (void) {"
                   --indent 2

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
setupPrint alloc len = do loopVar <- newLoopVar
                          line $ "int " ++ loopVar ++ ";"
                          line $ "for (" ++ loopVar ++ " = 0; " 
                              ++ loopVar ++ " < " ++ show len ++ "; "
                              ++ loopVar ++ "++ ) {"
                          line $ "printf(\"%d\\n\"," ++ alloc 
                              ++ "[" ++ loopVar ++ "] );"
                          line "}"
