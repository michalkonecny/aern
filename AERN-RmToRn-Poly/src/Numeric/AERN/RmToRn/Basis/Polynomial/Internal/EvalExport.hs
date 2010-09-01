{-# LANGUAGE ForeignFunctionInterface #-}

module Numeric.AERN.RmToRn.Basis.Polynomial.Internal.EvalExport where

import Foreign.StablePtr

type UunaryOp val = val -> val
type BinaryOp val = val -> val -> val
type ConvertOp v1 v2 = v1 -> v2

{-|
   Allow C programs to use a Haskell binary operator that has been
   sent to it via a StablePtr.  Its parameters and the result are opaque to C.
-}
eval_convert_hs :: 
        (StablePtr (ConvertOp v1 v2)) -> (StablePtr v1) -> IO (StablePtr v2) 
eval_convert_hs opSP v1SP =
    do
    op <- deRefStablePtr opSP
    v1 <- deRefStablePtr v1SP
    newStablePtr $ op v1

foreign export ccall eval_convert_hs :: 
        (StablePtr (ConvertOp v1 v2)) -> (StablePtr v1) -> IO (StablePtr v2) 
        
{-|
   Allow C programs to use a Haskell binary operator that has been
   sent to it via a StablePtr.  Its parameters and the result are opaque to C.
-}
eval_binary_hs :: 
        (StablePtr (BinaryOp val)) -> (StablePtr val) -> (StablePtr val) -> IO (StablePtr val) 
eval_binary_hs opSP v1SP v2SP =
    do
    op <- deRefStablePtr opSP
    v1 <- deRefStablePtr v1SP
    v2 <- deRefStablePtr v2SP
    newStablePtr $ op v1 v2

foreign export ccall eval_binary_hs :: 
        (StablePtr (BinaryOp val)) -> (StablePtr val) -> (StablePtr val) -> IO (StablePtr val)
        
{-|
   Allow C programs to free values produced by Haskell operations.
-}
free_hs :: (StablePtr a) -> IO ()
free_hs = freeStablePtr

foreign export ccall free_hs :: (StablePtr a) -> IO ()  

        