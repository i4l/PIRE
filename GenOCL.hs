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
import qualified Data.Map as M
-----------------------------------------------------------------------------
-- Show Instances

instance Show (Program a) where
  show p = unlines $ hostCode w
    where (_,w) = evalRWS (gen p) () emptyEnv

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
                           unless (t == TInt ) $ addParam $ show TInt ++ " arg" ++ show i ++ "c"
                           gen $ p ("arg" ++ show i)


genProg Skip = line ""

genProg (Assign name es e) = line $ show name --(Index name es) 
                          ++ concat [ "[" ++ show i ++ "]" | i <- es ]
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
genProg (Par start end f) = do let tid        = "tid"
                                   localSize  = "localSize"
                                   globalSize = "globalSize"
                                   f' = parForUnwind (f (var tid .+ ((var localSize) .* (var "ix")))) tid

                                   params = grabKernelParams f' 
                                   parameters = (init . concat) 
                                      [ " __global " ++ show (case t of TPointer _ -> t; a -> TPointer a) ++ " " ++  n ++ "," 
                                        | (n,t) <- params]

                               kerName <- fmap ((++) "k" . show) incVar
                               lineK $ "__kernel void " ++ kerName ++ "(" ++ parameters ++ " ) {"
                               kindent 2

                               lineK $ show TInt ++ " " ++  tid ++ " = " ++ "get_global_id(0)" ++ ";"
                               lineK $ show TInt ++ " " ++ localSize ++ " = " ++ "get_local_size(0);" 
                               lineK $ show TInt ++ " " ++ globalSize ++ " = " ++ "get_global_size(0);" 
                               lineK $ "if(" ++ tid ++ " < " ++ localSize ++ ") {"
                               kindent 2
                               lineK $ "for(int ix = 0; ix < " ++ globalSize ++ "/" ++ localSize ++ "; ix++) {"
                               kindent 2
                               genK f' []
                               kunindent 2
                               lineK "}"
                               kunindent 2
                               lineK "}"
                               runOCL kerName
                               setupOCLMemory params 0 end
                               launchKernel end (Num 1024)
                               modify $ \env -> env {kernelCounter = kernelCounter env + 1}
                               readOCL (grabKernelReadBacks f') end

                               kunindent 2
                               lineK "}"

genProg (For e1 e2 p) = do i <- newLoopVar
                           --line $ show TInt ++ " " ++ i ++ ";"
                           line $ "for(" ++ show TInt ++ " " ++ i ++ " = " ++ show e1 ++ "; " 
                               ++ i ++ " < " ++ show e2 ++ "; "
                               ++ i ++ "++) {"
                           indent 2
                           gen $ p (var i)
                           unindent 2
                           line "}"

genProg (Alloc t f) | t == TInt = error $ "Alloc on a scalar of type " ++ show t ++ ". Try Decl?"
                    | otherwise = 
                    do d <- incVar
                       let m = "mem" ++ show d
                           c = m ++ "c"
                           tc = case t of TPointer a -> a; a -> a
                           k = \dim -> Assign (var c) [] (head dim) .>>
                                        Statement $ var $ show t ++ " " ++ m ++ " = ("
                                        ++ show t ++ ") " ++ "malloc(sizeof(" ++ show tc ++ ") * " ++ c ++ ")"
                                       -- ++ "free(" ++ m ++ ");"

                       line $ show tc ++ " " ++ c ++ ";"
                       gen $ f m c k
                       --line $ "free(" ++ m ++ ");"
                        

--genProg (Alloc t dim f) = do d <- incVar
--                             let m = "mem" ++ show d
--                                 c = m ++ "c"
--                                 t' = case t of TPointer a -> a; a -> a;
--                             nestForAlloc dim m t
--                             line $ show t' ++ " " ++ c ++ ";" -- print size variable
--                             gen $ f m c 
                             --line $ "free(" ++ m ++ ");\n"

genProg (Decl t f)     = do d <- incVar
                            let m = "mem" ++ show d
                            line $ show t ++ " " ++ m ++ ";"
                            gen $ f m

-- Code gen in kernel code   
genK :: Program a -> [Name] -> Gen ()
genK Skip           ns = return ()
genK (Assign name es e) ns = lineK $ (show name)
                       ++ concat [ "[" ++ show i ++ "]" | i <- es ]
                       ++ " = " ++ show (derefScalar e ns) ++ ";"
genK (p1 :>> p2)   ns = genK p1 ns >> genK p2 ns
genK (If c p1 Skip) ns = do lineK $ "if(" ++ show (derefScalar c ns) ++ ") {"
                            kindent 2
                            genK p1 ns
                            kunindent 2
                            lineK "}"
genK (If c p1 p2) ns = do lineK $ "if(" ++ show (derefScalar c ns) ++ ") { "
                          kindent 2
                          genK p1 ns
                          kunindent 2
                          lineK "else { "
                          kindent 2
                          genK p2 ns
                          kunindent 2
                          lineK "}"
genK (For e1 e2 p) ns   = do i <- newLoopVar
                             --lineK $ show TInt ++ " " ++ i ++ ";"
                             lineK $ "for(" ++ show TInt ++ i ++ " = " ++ show (derefScalar e1 ns) ++ "; " 
                                 ++ i ++ " < " ++ show (derefScalar e2 ns) ++ "; "
                                 ++ i ++ "++ ) {"
                             kindent 2
                             genK (p (var i)) ns
                             kunindent 2
                             lineK "}"
genK (Par start end f) ns = genK (For start end f) ns
genK (Decl t f)        ns = do d <- incVar
                               let m = "mem" ++ show d
                               lineK $ show t ++ " " ++ m ++ ";"
                               genK (f m) (m:ns)

genK (Alloc t f) ns = error "Alloc in Kernel code not allowed"
                       -- do argName <- fmap ((++) "mem" . show) incVar
                       --   lineK $ "// Alloc in Kernel"
                       --   genK $ f argName (argName ++ "c")



-----------------------------------------------------------------------------
-- Other things that may need revising.

setupOCLMemory :: Parameters -> Int -> Size -> Gen ()
setupOCLMemory []           i s = return ()
setupOCLMemory ((n,t):xs) i sz = let s = sz
                                     isScalar = case t of TPointer _ -> False; _ -> True
                                  in do nameUsed <- nameExists n -- If a name is already declared we can reuse it
                                        let objPostfix = "_obj"
                                            createBuffers = 
                                                     (if not nameUsed then "cl_mem " else "") ++ n ++ 
                                                     objPostfix ++ " = clCreateBuffer(context, " ++ 
                                                     "CL_MEM_READ_WRITE" ++ ", " ++ (if isScalar then "" else show s ++ "*") ++ "sizeof(" ++ 
                                                     removePointers t ++ "), NULL, NULL);"
                                        line createBuffers
                                        let copyBuffers = "clEnqueueWriteBuffer(command_queue, " ++ n ++ 
                                                          objPostfix ++ ", CL_TRUE, 0, " ++ (if isScalar then "" else show s ++ "*") ++ "sizeof(" ++ 
                                                          removePointers t ++"), " ++ 
                                                          (if isScalar then "&" else "") ++ n 
                                                          ++ ", 0, NULL, NULL);"

                                        -- set kernel arguments
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

--launchKernel :: Int -> Int -> Gen ()
launchKernel :: Expr -> Expr -> Gen ()
launchKernel global local = do
  kcount <- gets kernelCounter
  line $ (if kcount <= 0 then "size_t " else "") ++ "global_item_size = " ++ show global ++ ";"
  line $ (if kcount <= 0 then "size_t " else "") ++ "local_item_size = "  ++ show local ++ ";"
  line "clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);"

--readOCL :: Name -> Type -> Size -> Gen () 
readOCL :: Parameters -> Size -> Gen () 
readOCL []            _  = return ()
readOCL ((n,t):xs) sz | n `elem` reservedNames = readOCL xs sz
                      | otherwise = 
                        let s = sz
                        in do line $ "clEnqueueReadBuffer(command_queue, " ++ n ++ "_obj" ++ ", CL_TRUE, 0, " ++
                                show s ++ "*sizeof(" ++ removePointers t ++ "), " ++ n ++ ", 0, NULL, NULL);\n\n"
                              readOCL xs sz
                                
------------------------------------------------------------
-- Extras

setupHeadings :: Gen ()
setupHeadings = do tell $ mempty {pre = ["#include <stdio.h>"]}
                   tell $ mempty {pre = ["#include <stdlib.h>"]}
                   tell $ mempty {pre = ["#ifdef __APPLE__"
                                        ,"#include <OpenCL/opencl.h>"
                                        ,"#else"
                                        ,"#include <CL/cl.h>"
                                        ,"#endif"]}
                   tell $ mempty {pre = ["#include <math.h>"]} -- note: remember to link math. -lm
                   tell $ mempty {pre = ["#include <time.h>"]}
                   tell $ mempty {pre = ["#include <string.h>"]} 
                   tell $ mempty {pre = ["#include \"feldspar_c99.h\""]}

                   tell $ mempty {pre = ["#define MAX_SOURCE_SIZE (0x100000)\n\n"]}

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

