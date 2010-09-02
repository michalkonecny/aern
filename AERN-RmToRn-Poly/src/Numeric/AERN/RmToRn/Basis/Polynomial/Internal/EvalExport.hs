{-# LANGUAGE ForeignFunctionInterface #-}
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.Internal.EvalExport
    Description :  functions for use in C for coefficient arithmetic
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Functions for use in C for coefficient arithmetic.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.Internal.EvalExport 
(
        eval_convert_hs,
        eval_unary_hs,
        eval_binary_hs,
        free_SP_hs,
--        ,
--        new_Ptr_hs,
--        free_Ptr_hs,
--        eval_const_inplace_hs,
--        eval_unary_inplace_hs,
--        eval_binary_inplace_hs
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.FFIhelper

--import Foreign.Ptr
import Foreign.StablePtr


{-|
   Allow C programs to use a Haskell binary operator that has been
   sent to it via a StablePtr.  Its parameters and the result are opaque to C.
-}
{-# INLINE eval_convert_hs #-}
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
   Allow C programs to use a Haskell unary operator that has been
   sent to it via a StablePtr.  Its parameters and the result are opaque to C.
-}
eval_unary_hs :: 
        (StablePtr (UnaryOp val)) -> (StablePtr val) -> IO (StablePtr val) 
eval_unary_hs opSP v1SP =
    do
    op <- deRefStablePtr opSP
    v1 <- deRefStablePtr v1SP
    newStablePtr $ op v1

foreign export ccall eval_unary_hs :: 
        (StablePtr (UnaryOp val)) -> (StablePtr val) -> IO (StablePtr val)
        
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
    let result = op v1 v2
    newStablePtr result

foreign export ccall eval_binary_hs :: 
        (StablePtr (BinaryOp val)) -> (StablePtr val) -> (StablePtr val) -> IO (StablePtr val)
        
{-|
   Allow C programs to free values produced by Haskell operations.
-}
free_SP_hs :: (StablePtr a) -> IO ()
free_SP_hs = freeStablePtr

foreign export ccall free_SP_hs :: (StablePtr a) -> IO ()  

        