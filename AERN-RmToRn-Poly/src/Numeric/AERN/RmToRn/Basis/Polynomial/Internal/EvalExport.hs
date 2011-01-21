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
()
where

import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics 

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering
import Numeric.AERN.Basics.Mutable

--import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly

--import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.Types

import Control.Monad.ST (stToIO, RealWorld, runST, unsafeSTToIO)


{-|
   Allow C programs to use a Haskell type conversion operator that has been
   sent to it as a StablePtr.  Its parameter and result are opaque to C.
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
   Allow C programs to use a Haskell type conversion operator that has been
   sent to it as a StablePtr.  Its result is opaque to C.
-}
{-# INLINE eval_convertFromDouble_hs #-}
eval_convertFromDouble_hs :: 
        (StablePtr (ConvertFromDoubleOp v2)) -> CDouble -> IO (StablePtr v2) 
eval_convertFromDouble_hs opSP dC =
    do
    op <- deRefStablePtr opSP
    let d = cDouble2Double dC
    newStablePtr $ op d

foreign export ccall eval_convertFromDouble_hs :: 
        (StablePtr (ConvertFromDoubleOp v2)) -> CDouble -> IO (StablePtr v2) 
        
{-|
   Allow C programs to use a Haskell comparison operator that has been
   sent to it as a StablePtr.  Its parameters are opaque to C.
-}
{-# INLINE eval_compare_hs #-}
eval_compare_hs :: 
        (StablePtr (ComparisonOp val)) -> (StablePtr val) -> (StablePtr val) -> IO CInt 
eval_compare_hs compSP v1SP v2SP =
    do
    comp <- deRefStablePtr compSP
    v1 <- deRefStablePtr v1SP
    v2 <- deRefStablePtr v2SP
    return $ pord2int $ comp v1 v2
    where
    pord2int (Just EQ) = 0
    pord2int (Just LT) = -1
    pord2int (Just LEE) = -1
    pord2int (Just GT) = 1
    pord2int (Just GEE) = 1
    pord2int _ = 0 
       -- consider incomparable or hard to compare pairs as equal...;
       -- this is OK when this is used for ordering polynomial coefficients

foreign export ccall eval_compare_hs :: 
        (StablePtr (ComparisonOp val)) -> (StablePtr val) -> (StablePtr val) -> IO CInt 
        
{-|
   Allow C programs to use a Haskell unary operator that has been
   sent to it as a StablePtr.  Its parameter and result are opaque to C.
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
   sent to it as a StablePtr.  Its parameters and result are opaque to C.
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
   Allow C programs to create cloned SP values.
-}
clone_SP_hs :: (StablePtr t) -> IO (StablePtr t)
clone_SP_hs sampleSP =
    do
    sample <- deRefStablePtr sampleSP
    newStablePtr sample
     
foreign export ccall clone_SP_hs :: (StablePtr t) -> IO (StablePtr t)  
        
{-|
   Allow C programs to free values produced by Haskell operations.
-}
free_SP_hs :: (StablePtr t) -> IO ()
free_SP_hs = freeStablePtr

foreign export ccall free_SP_hs :: (StablePtr t) -> IO ()  


{-|
   Allow C programs to use a Haskell variable creation operator that has been
   sent to it as a StablePtr.  Its parameter and result are opaque to C.
-}
eval_newMutable_hs ::
    (StablePtr (NewOpMutable s t)) -> 
    (StablePtr t) -> 
    IO (StablePtr (Mutable t s)) 
eval_newMutable_hs opSP valSP =
    do
    op <- deRefStablePtr opSP
    val <- deRefStablePtr valSP
    var <- unsafeSTToIO $ op val
    newStablePtr var

foreign export ccall eval_newMutable_hs ::
    (StablePtr (NewOpMutable s t)) -> 
    (StablePtr t) -> 
    IO (StablePtr (Mutable t s)) 


{-|
   Allow C programs to use a Haskell variable clone operator that has been
   sent to it as a StablePtr.  Its parameter and result are opaque to C.
-}
eval_cloneMutable_hs ::
    (StablePtr t) -> 
    (StablePtr (CloneOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    IO (StablePtr (Mutable t s)) 
eval_cloneMutable_hs _ opSP oldSP =
    do
    op <- deRefStablePtr opSP
    old <- deRefStablePtr oldSP
    var <- unsafeSTToIO $ op old
    newStablePtr var

foreign export ccall eval_cloneMutable_hs ::
    (StablePtr t) -> 
    (StablePtr (CloneOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    IO (StablePtr (Mutable t s)) 

{-|
   Allow C programs to set the value of a Haskell variable.
-}
eval_assignMutableFromPure_hs ::
    (StablePtr (UnaryFromPureOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr t) -> 
    IO () 
eval_assignMutableFromPure_hs opSP resSP valSP =
    do
    op <- deRefStablePtr opSP
    res <- deRefStablePtr resSP
    val <- deRefStablePtr valSP
    unsafeSTToIO $ op res val

foreign export ccall eval_assignMutableFromPure_hs ::
    (StablePtr (UnaryFromPureOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr t) -> 
    IO () 

{-|
   Allow C programs to copy the value among Haskell variables.
-}
eval_assignMutable_hs ::
    (StablePtr t) -> 
    (StablePtr (UnaryOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr (Mutable t s)) -> 
    IO () 
eval_assignMutable_hs _ opSP resSP varSP =
    do
    op <- deRefStablePtr opSP
    res <- deRefStablePtr resSP
    var <- deRefStablePtr varSP
    unsafeSTToIO $ op res var

foreign export ccall eval_assignMutable_hs ::
    (StablePtr t) -> 
    (StablePtr (UnaryOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr (Mutable t s)) -> 
    IO () 


{-|
   Allow C programs to use a Haskell in-place unary operator that has been
   sent to it as a StablePtr.  Its parameter and result are opaque to C.
-}
eval_unaryMutable_hs ::
    (StablePtr t) -> 
    (StablePtr (UnaryOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr (Mutable t s)) -> 
    IO () 
eval_unaryMutable_hs _ opSP resSP v1SP =
    do
    op <- deRefStablePtr opSP
    res <- deRefStablePtr resSP
    v1 <- deRefStablePtr v1SP
    unsafeSTToIO $ op res v1

foreign export ccall eval_unaryMutable_hs ::
    (StablePtr t) -> 
    (StablePtr (UnaryOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr (Mutable t s)) -> 
    IO ()  

    
{-|
   Allow C programs to use a Haskell in-place binary operator that has been
   sent to it as a StablePtr.  Its patameters and result are opaque to C.
-}
eval_binaryMutable_hs :: 
    (StablePtr t) -> 
    (StablePtr (BinaryOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr (Mutable t s)) -> 
    IO () 
eval_binaryMutable_hs _ opSP resSP v1SP v2SP =
    do
    op <- deRefStablePtr opSP
    res <- deRefStablePtr resSP
    v1 <- deRefStablePtr v1SP
    v2 <- deRefStablePtr v2SP
    unsafeSTToIO $ op res v1 v2

foreign export ccall eval_binaryMutable_hs :: 
    (StablePtr t) -> 
    (StablePtr (BinaryOpMutable s t)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr (Mutable t s)) -> 
    (StablePtr (Mutable t s)) -> 
    IO ()  
    
        