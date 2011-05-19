{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
#include <GenericCoeff/poly.h>
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate
    Description :  evaluation functions for C polynomials with Haskell coefficients
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Evaluation functions for C polynomials with Haskell coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Evaluate
where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly

import Numeric.AERN.Misc.Debug

import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Data.Word

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST, unsafeIOToST, unsafeSTToIO)

import System.IO.Unsafe

import Foreign.Ptr(Ptr,nullPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Marshal.Array (newArray)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import qualified Foreign.Concurrent as Conc (newForeignPtr)

--import Data.Typeable(Typeable)
--import Data.Function(on)
    

foreign import ccall safe "evalAtPtChebBasisGenCf"
        poly_evalAtPtChebBasis :: 
            (Ptr (CPoly cf)) -> 
            (Ptr (StablePtr val)) -> 
            (StablePtr val) ->
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) ->
            (StablePtr (ConvertOp (Mutable cf s) val)) ->
            IO (StablePtr val)

evalAtPtChebBasis :: 
    (Storable cf, CanBeMutable cf) => 
    (Poly cf) ->
    [val] {-^ values to substitute for variables @[0..(maxArity-1)]@ -} ->
    val {-^ number @1@ -} ->
    (BinaryOp val) {-^ addition -} -> 
    (BinaryOp val) {-^ subtraction -} -> 
    (BinaryOp val) {-^ multiplication -} -> 
    (ConvertOp cf val) ->
    val
evalAtPtChebBasis (Poly _ polyFP) vals one add subtr mult cf2val =
    unsafePerformIO $
    do
    valSPs <- mapM newStablePtr vals
    valSPsPtr <- newArray valSPs
    oneSP <- newStablePtr one
    addSP <- newStablePtr add
    subtrSP <- newStablePtr subtr
    multSP <- newStablePtr mult
    cfm2valSP <- newStablePtr cfm2val
    resSP <- withForeignPtr polyFP $ \polyPtr ->
        poly_evalAtPtChebBasis polyPtr valSPsPtr oneSP addSP subtrSP multSP cfm2valSP
    freeStablePtr oneSP
    _ <- mapM freeStablePtr [addSP, subtrSP, multSP]
    free valSPsPtr
    _ <- mapM freeStablePtr valSPs
    res <- deRefStablePtr resSP 
    freeStablePtr resSP
    return res
    where
    cfm2val cfM =
        unsafePerformIO $
        do
        cf <- unsafeSTToIO $ unsafeReadMutable cfM
        return $ cf2val cf

foreign import ccall safe "evalAtPtPowerBasisGenCf"
        poly_evalAtPtPowerBasis :: 
            (Ptr (CPoly cf)) -> 
            (Ptr (StablePtr val)) -> 
            (StablePtr val) ->
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) ->
            (StablePtr (ConvertOp (Mutable cf s) val)) ->
            IO (StablePtr val)  

evalAtPtPowerBasis :: 
    (Storable cf, CanBeMutable cf) => 
    (Poly cf) ->
    [val] {-^ values to substitute for variables @[0..(maxArity-1)]@ -} ->
    val {-^ number @1@ -} ->
    (BinaryOp val) {-^ addition -} -> 
    (BinaryOp val) {-^ multiplication -} -> 
    (ConvertOp cf val) ->
    val
evalAtPtPowerBasis (Poly _ polyFP) vals one add mult cf2val =
    unsafePerformIO $
    do
    valSPs <- mapM newStablePtr vals
    valSPsPtr <- newArray valSPs
    oneSP <- newStablePtr one
    addSP <- newStablePtr add
    multSP <- newStablePtr mult
    cfm2valSP <- newStablePtr cfm2val
    resSP <- withForeignPtr polyFP $ \polyPtr ->
        poly_evalAtPtPowerBasis polyPtr valSPsPtr oneSP addSP multSP cfm2valSP
    freeStablePtr oneSP
    _ <- mapM freeStablePtr [addSP, multSP]
    free valSPsPtr
    _ <- mapM freeStablePtr valSPs
    res <- deRefStablePtr resSP 
    freeStablePtr resSP
    return res
    where
    cfm2val cfM =
        unsafePerformIO $
        do
        cf <- unsafeSTToIO $ unsafeReadMutable cfM
        return $ cf2val cf

----------------------------------------------------------------

foreign import ccall safe "boundUpThinGenCf"
    poly_boundUpThin ::
        OpsPtr cf ->
        (StablePtr (Mutable cf s)) -> 
        (Ptr (CPoly cf)) -> 
        IO ()

polyBoundUpThin :: 
    (HasZero cf, NumOrd.PartialComparison cf, CanBeMutable cf) =>
    Mutable cf s ->
    PolyM cf s ->
    ST s ()
polyBoundUpThin =
    polyEval poly_boundUpThin

foreign import ccall safe "boundDnThinGenCf"
    poly_boundDnThin ::
        OpsPtr cf ->
        (StablePtr (Mutable cf s)) -> 
        (Ptr (CPoly cf)) -> 
        IO ()

polyBoundDnThin :: 
    (HasZero cf, NumOrd.PartialComparison cf, CanBeMutable cf) =>
    Mutable cf s ->
    PolyM cf s ->
    ST s ()
polyBoundDnThin =
    polyEval poly_boundDnThin

foreign import ccall safe "boundUpGenCf"
    poly_boundUp ::
        OpsPtr cf ->
        (StablePtr (Mutable cf s)) -> 
        (Ptr (CPoly cf)) -> 
        IO ()

polyBoundUp :: 
    (HasZero cf, NumOrd.PartialComparison cf, CanBeMutable cf) =>
    Mutable cf s ->
    PolyM cf s ->
    ST s ()
polyBoundUp =
    polyEval poly_boundUp

foreign import ccall safe "boundDnGenCf"
    poly_boundDn ::
        OpsPtr cf ->
        (StablePtr (Mutable cf s)) -> 
        (Ptr (CPoly cf)) -> 
        IO ()

polyBoundDn :: 
    (HasZero cf, NumOrd.PartialComparison cf, CanBeMutable cf) =>
    Mutable cf s ->
    PolyM cf s ->
    ST s ()
polyBoundDn =
    polyEval poly_boundDn

polyEval evalOp resM (PolyM (Poly opsFP pFP)) =
    unsafeIOToST $
    do
    resMST <- newStablePtr resM
    withForeignPtr pFP $ \p ->
      withForeignPtr opsFP $ \opsP ->
        evalOp opsP resMST p
    freeStablePtr resMST
