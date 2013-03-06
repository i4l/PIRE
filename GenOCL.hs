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


gen :: Program a -> Gen ()

gen Skip = line ""

gen (Assign name es e) = line $ show (Index name es) 
                      ++ " = " ++ show e ++ ";"

gen (p1 :>> p2) = gen p1 >> gen p2

gen (If c p1 p2) = do line $ "if( " ++ show c ++ " ) { "
                      indent 2
                      gen p1
                      unindent 2
                      line "else { "
                      indent 2
                      gen p2
                      unindent 2
                      line "}"

gen (Par start end f) = gen $ For start end f


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
                         when (length dim > 1) $ nestForAlloc dim m t
                         gen $ f (locNest m) (Index m)
                         line $ "free(" ++ m ++ ");\n"
 

---gen (Alloc siz f) = do 
--   d <- incVar
--   let m = "mem" ++ show d
--   line $ m ++ " = malloc(" ++ show siz ++ ");"
--   gen $ f (locArray m) (array m siz)
--   line $ "free(" ++ m ++ ");"


--gen (AllocNew t siz arr f) = do
--  let objPostfix = "_obj"
--      memPrefix  = "mem"
--
--  -- Allocate for argument
--  d <- incVar
--  let m = "mem" ++ show d
--  line $ show t ++ " " ++ m ++ " = (" ++ show t ++ ") malloc(" ++
--          "sizeof(" ++ show t ++ ")*" ++ show siz ++ ");"
--  addInitFunc d (pull $ doit arr)
--
--  -- Allocate for result
--  kernInfo <- (genKernel f [(m, d)] arr False)
--  let resID = resultID kernInfo
--  line $ show t ++ " " ++ memPrefix ++ show resID ++ " = (" ++ show t ++ ") malloc(" ++ 
--          "sizeof(" ++ removePointer t ++ ")*" ++ show siz ++ ");\n\n"
--
--  -- fetch the Map, so we have something to work with
--  allocMap <- fmap Map.toList getHostAllocMap
-- 
--  -- initialize allocated arrays
--  initFuncs <- fmap Map.toList getInitFuncs
--  v <- incVar
--  let len          = size $ arr
--      loopVar      = ([ "i", "j", "k" ] ++ [ "i" ++ show i | i <- [0..] ]) !! v
--      allocStrings = map (\(h,f) -> Assign 
--                                      (memPrefix ++ show h) 
--                                      [var loopVar] (f (var loopVar))) initFuncs
--  line $ "int " ++ loopVar ++ ";"
--  line $ "for ( " ++ loopVar ++ " = 0; " ++ loopVar ++ " < " ++ show len ++ "; " ++  loopVar ++ "++ ) {"
--  indent 2
--  mapM_ gen allocStrings
--  unindent 2
--  line "}"
--  
--  -- create cl_mem buffers
--  let createBuffers (h,k) = "cl_mem " ++ memPrefix ++ show h ++ objPostfix ++ " = clCreateBuffer(context, " ++ 
--                (if k /= 0 then "CL_MEM_READ_ONLY" else "CL_MEM_WRITE_ONLY") ++
--                ", " ++ show siz ++ "*sizeof(" ++ removePointer t ++ "), NULL, NULL);"
--  mapM_ line (map createBuffers allocMap)
--
--  -- copy data to cl_mem buffers
--  let copyBuffers (h,_) = "clEnqueueWriteBuffer(command_queue, " ++ memPrefix ++ show h ++ 
--                          objPostfix ++ ", CL_TRUE, 0, " ++ show siz ++ " * sizeof(" ++ 
--                          removePointer t ++"), " ++ memPrefix ++ show h ++ ", 0, NULL, NULL);"
--  resAlloc <- fmap fromJust $  lookupForKernel 0 
--  let removeRes = delete (resAlloc, 0) -- we don't want to copy the result array to the GPU.
--  mapM_ line (map copyBuffers (removeRes allocMap))
--
--  -- create kernel & build program
--  line $ "cl_program program = clCreateProgramWithSource(context, 1, (const char **)&source_str, " ++
--         "(const size_t *)&source_size, NULL);"
--  line "clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);"
--  line "cl_kernel kernel = clCreateKernel(program, \"k0\", NULL);" 
--  
--  -- set arguments to kernel
--  let setArgs (h,k) = "clSetKernelArg(kernel, " ++ show k ++ 
--                      ", sizeof(cl_mem), (void *)&" ++ memPrefix ++ show h ++ objPostfix ++ ");"
--  mapM_ line (map setArgs allocMap)
--
--  -- launch kernel
--  line $ "size_t global_item_size = " ++ show siz ++ ";"
--  line $ "size_t local_item_size = "  ++ show siz ++ ";"
--  line "clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);"
--
--  --read back to result array
--  line $ "clEnqueueReadBuffer(command_queue, " ++ memPrefix ++ show resID ++ objPostfix ++ ", CL_TRUE, 0, " ++
--          show siz ++ "* sizeof(" ++ removePointer t ++ "), " ++ memPrefix ++ show resID ++ ", 0, NULL, NULL);\n\n"


------------------------------------------------------------
-- Kernel generation

--data Kernel = Kernel {resultID :: Int}
--
---- Assumption: param 0 is the result array.
---- The Bool is for deciding if to allocate the resulting array (set False by FIRST called).
--genKernel :: (Loc Expr a -> Array Pull Expr -> Program a) -> [(Name, Int)] -> Array Pull Expr -> Bool -> Gen Kernel
--genKernel f names arr isCalledNested = do
--  let arrPrefix = "arr"
--  v0 <- incVar
--  k0 <- if isCalledNested then return (-1) else addKernelParam v0 -- TODO find a way of fixing this ugly thing.
--  
--  -- Create a new array which is used inside the kernel.
--  -- Pass it to the function f which takes Loc and Array.
--  let res = "arr" ++ show k0
--  k1 <- addKernelParam (snd $ head names)
--  let kernelArr = array (arrPrefix ++ show k1) (size arr)
--  genKernel' $ f (locArray res $ var "tid") kernelArr
--
--  newHostMem <- lookupForHost k0 -- k0 is the result array
--  return $ Kernel (fromJust newHostMem)
--  where
--    -- This is the dual to Gen - for Programs in kernels.
--    genKernel' :: Program a -> Gen ()
--    genKernel' Skip = lineK "0;"
--    genKernel' (Assign name es e) = lineK $ show (Index name es) ++ " = " ++ show e ++ ";"
--    genKernel' (p1 :>> p2) = genKernel' p1 >> genKernel' p2 
--    genKernel' (If c p1 p2) = do
--      lineK $ "if( " ++ show c ++ " ) { "
--      indent 2
--      genKernel' p1
--      unindent 2
--      lineK "else { "
--      indent 2
--      genKernel' p2
--      unindent 2
--      lineK "}"
--
--    genKernel' (Par _ bound p) = do
--      let tid     = "tid"
--      let kerName = 'k' : show (0 :: Int) -- TODO fix (might have multiple kernels)
--
--      paramMapSize <- fmap Map.size (gets paramMap)
--      let removeLastComma = reverse . drop 1 . reverse
--          arrPrefix       = "arr"
--          parameters      = (removeLastComma . concat) 
--                            [ " __global int *" ++ arrPrefix ++ show i ++ "," | i <- [0.. paramMapSize-1]]
--
--      lineK $ "__kernel void " ++ kerName ++ "(" ++ parameters ++ " ) {"
--      lineK "int tid = get_global_id(0);"
--      lineK $ "if( tid < " ++ show bound ++ " ) {"
--      genKernel' $ p (var tid)
--
--      lineK "}"
--      lineK "}"
--
--    genKernel' (For e1 e2 p) = do
--      d <- incVar
--      let i = ([ "i", "j", "k" ] ++ [ "i" ++ show i | i <- [0..] ]) !! d
--      lineK $ show TInt ++ " " ++ i ++ ";"
--      lineK $ "for( " ++ i ++ " = " ++ show e1 ++ "; " 
--                      ++ i ++ " < " ++ show e2 ++ "; " ++ i ++ "++ ) {"
--      indent 2
--      genKernel' (p (var i))
--      unindent 2
--      lineK "}"
--
--    --genKernel' (Alloc siz f) = do 
--    --  d <- incVar
--    --  let m = "mem" ++ show d
--    --  lineK $ m ++ " = malloc(" ++ show siz ++ ");" -- TODO needs a type cast before malloc?
--    --  genKernel' $ f (locArray m) (array m siz)
--    --  lineK $ "free(" ++ m ++ ");"
--
--    -- The internal version of AllocNew (that happends within another AllocNew).
--    --genKernel' (AllocNew t siz arr f) = do
--    --  d <- incVar
--    --  addInitFunc d (pull $ doit arr)
--    --  let m = "mem" ++ show d
--    --  line $ show t ++ " " ++ m ++ " = (" ++ show t ++ ") malloc(" ++ "sizeof(" ++ show t ++ ")*" ++ show siz ++ ");"
--    --  genKernel f [(m, d)] arr True 
--    --  return ()
--
--    

------------------------------------------------------------
-- Extras

setupHeadings :: Gen ()
setupHeadings = do line "#include <stdio.h>"
                   line "#include <stdlib.h>"
                   --line "#include <CL/cl.h>"
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
