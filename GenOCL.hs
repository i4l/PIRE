module GenOCL where

import Util
import Program
import Types
import Expr
import Gen

--import qualified Data.Map as Map
--import Data.Maybe
--import Control.Monad.State
import Control.Monad
import Data.List


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
                               paramTriples = (nubBy (\(n,_,_) (m,_,_) -> n == m) . params) $ grabKernelParams (f $ var tid)
                               parameters = (init . concat) [ " __global " ++ show t ++ " " ++  n ++ "," | (n,dim,t) <- paramTriples]

                           line "//Param triples"
                           mapM_ line $ map ((++) "// " . show) (paramTriples)

                           kerName <- fmap ((++) "k" . show) incVar
                           lineK $ "__kernel void " ++ kerName ++ "(" ++ parameters ++ " ) {"
                           lineK $ show TInt ++ " " ++  tid ++ " = " ++ "get_global_id(0)" ++ ";"
                           lineK $ "if( tid < " ++ show end ++ " ) {"
                           kdata <- genK $ f (var tid)
                           line $ "// Run parallel loop from host"

                           runOCL kerName
                           setupOCLMemory paramTriples 0 end
                           launchKernel 2048 64
                           let (n,dim,t) = head paramTriples
                           readOCL n (TPointer t) end
                           lineK "}"
                           lineK "}"
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
                         indent 2
                         genK p1
                         unindent 2
genK (If c p1 p2) = do lineK $ "if( " ++ show c ++ " ) { "
                       indent 2
                       genK p1
                       unindent 2
                       lineK "else { "
                       indent 2
                       genK p2
                       unindent 2
                       lineK "}"
genK (For e1 e2 p) = do i <- fmap fst newLoopVar
                        lineK $ show TInt ++ " " ++ i ++ ";"
                        lineK $ "for( " ++ i ++ " = " ++ show e1 ++ "; " 
                            ++ i ++ " < " ++ show e2 ++ "; "
                            ++ i ++ "++ ) {"
                        indent 2
                        genK $ p (var i)
                        unindent 2
                        lineK "}"
genK (Par start end f) = error "Par"
genK (Alloc t dim f) = do kerName <- fmap ((++) "k" . show) incVar
                          argName <- fmap ((++) "mem" . show) incVar
                          lineK $ "// Alloc in Kernel"
                          kdata <- genK $ f (locNest argName) (Index argName)
                          --return $ KData $ (argName,dim,t) : params kdata
                          return ()



-- Analysis of AST - Gets the names of arrays used in the AST
grabKernelParams :: Program a -> KData
grabKernelParams (Assign name es e) = let lhs = (name,es,typeNest TInt es)
                                          rhs = getKDataExpr e
                                      in (KData $ lhs:( params rhs))
grabKernelParams (a :>> b) = grabKernelParams a +++ grabKernelParams b
  where a +++ b = KData $ (params a) ++ (params b)
grabKernelParams (If c tb fb) = let cond   = getKDataExpr c 
                                    bodies = grabKernelParams $ tb :>> fb
                                in KData $  params cond ++ params bodies
grabKernelParams (For start end f) = grabKernelParams $ f (var "tid")--error "for"
grabKernelParams (Par start end f) = error "par"
grabKernelParams (Alloc t dim p)   = error "alloc"
grabKernelParams (Print t e)       = error "print"
grabKernelParams _                 = KData []


getKDataExpr :: Expr -> KData
getKDataExpr (Index a is) = KData [(a,is, typeNest TInt is)]
getKDataExpr (a :+: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :-: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :/: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :%: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :*: b)    = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr (a :<=: b)   = KData $ params (getKDataExpr a) ++ params (getKDataExpr b)
getKDataExpr _            = KData []


setupOCLMemory :: [(Name,Dim,Type)] -> Int -> Size -> Gen ()
setupOCLMemory []           i s = return ()
setupOCLMemory ((n,d,t):xs) i s = do let objPostfix = "_obj"
                                         createBuffers = "cl_mem " ++ n ++ objPostfix ++ " = clCreateBuffer(context, " ++ 
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
                                     when (i /= 0)$ line copyBuffers
                                     setupOCLMemory xs (i+1) s

runOCL :: Name -> Gen ()
runOCL kname = do --create kernel & build program
            line $ "cl_program program = clCreateProgramWithSource(context, 1, (const char **)&source_str, " ++
                   "(const size_t *)&source_size, NULL);"
            line "clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);"
            line $ "cl_kernel kernel = clCreateKernel(program, \"" ++ kname ++ "\", NULL);" 

launchKernel :: Int -> Int -> Gen ()
launchKernel global local = do 
  line $ "size_t global_item_size = " ++ show global ++ ";"
  line $ "size_t local_item_size = "  ++ show local ++ ";"
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
