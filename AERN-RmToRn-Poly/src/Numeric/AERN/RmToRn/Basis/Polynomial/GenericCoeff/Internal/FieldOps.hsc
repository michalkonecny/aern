{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
#include <GenericCoeff/poly.h>
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.FieldOps
    Description :  field operations for C polynomials with Haskell coefficients
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations for C polynomials with Haskell coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.FieldOps
where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly

import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST, unsafeIOToST, unsafeSTToIO)

import Numeric.AERN.Misc.Debug

import System.IO.Unsafe

import Foreign.Ptr(Ptr,nullPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Marshal.Array (newArray)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import qualified Foreign.Concurrent as Conc (newForeignPtr)


foreign import ccall safe "addUpGenCf"
        poly_addUp :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

foreign import ccall safe "addDnGenCf"
        poly_addDn :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

polyAddUp ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    (Ptr (Ops_Mutable s cf)) ->
    PolyFP cf s ->
    PolyFP cf s ->
    PolyFP cf s ->
    ST s ()
polyAddUp sample opsPtr = 
    polyBinaryOp poly_addUp sample opsPtr

polyAddDn ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    (Ptr (Ops_Mutable s cf)) ->
    PolyFP cf s ->
    PolyFP cf s ->
    PolyFP cf s ->
    ST s ()
polyAddDn sample opsPtr = 
    polyBinaryOp poly_addDn sample opsPtr

----------------------------------------------------------------

foreign import ccall safe "scaleUpThinGenCf"
        poly_scaleUpThin :: 
            (StablePtr cf) ->
            (Ptr (Ops_Mutable s cf)) ->
            (StablePtr (Mutable cf s)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

foreign import ccall safe "scaleDnThinGenCf"
        poly_scaleDnThin :: 
            (StablePtr cf) ->
            (Ptr (Ops_Mutable s cf)) ->
            (StablePtr (Mutable cf s)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

foreign import ccall safe "scaleEnclGenCf"
        poly_scaleEncl :: 
            (Ptr (Ops_Mutable s cf)) ->
            (StablePtr (Mutable cf s)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

polyScaleUp ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    (Ptr (Ops_Mutable s cf)) ->
    cf ->
    PolyFP cf s ->
    ST s ()
polyScaleUp sample opsPtr = 
    polyScalingOp poly_scaleUpThin sample opsPtr

polyScaleDn ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    (Ptr (Ops_Mutable s cf)) ->
    cf ->
    PolyFP cf s ->
    ST s ()
polyScaleDn sample opsPtr = 
    polyScalingOp poly_scaleDnThin sample opsPtr

polyScaleEncl ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    (Ptr (Ops_Mutable s cf)) ->
    cf ->
    PolyFP cf s ->
    ST s ()
polyScaleEncl opsPtr = 
    polyScalingOpNoZero poly_scaleEncl opsPtr

----------------------------------------------------------------

polyScalingOp scalingOp sample opsPtr scalingFactor (PolyFP pFP) =
    do
    sfM <- unsafeMakeMutable scalingFactor 
    unsafeIOToST $
      do
      zeroSP <- newStablePtr $ head [zero, sample]
      factorSP <- newStablePtr sfM
      _ <- withForeignPtr pFP $ \pP ->
            scalingOp zeroSP opsPtr factorSP pP
      return ()

----------------------------------------------------------------

polyScalingOpNoZero scalingOp opsPtr scalingFactor (PolyFP pFP) =
    do
    sfM <- unsafeMakeMutable scalingFactor 
    unsafeIOToST $
      do
      factorSP <- newStablePtr sfM
      _ <- withForeignPtr pFP $ \pP ->
            scalingOp opsPtr factorSP pP
      return ()

--------------------------------------------------------------

polyBinaryOp binaryOp sample opsPtr 
        (PolyFP resFP) (PolyFP p1FP) (PolyFP p2FP) =
    unsafeIOToST $
    do
    zeroSP <- newStablePtr $ head [zero, sample]
    compareSP <- newStablePtr compareMutable
    _ <- withForeignPtr p1FP $ \p1P ->
             withForeignPtr p2FP $ \p2P ->
                 withForeignPtr resFP $ \resP ->
                     binaryOp zeroSP compareSP opsPtr resP p1P p2P
    freeStablePtr compareSP
    where
    compareMutable v1M v2M =
        unsafePerformIO $ unsafeSTToIO $
        do
        v1 <- unsafeReadMutable v1M
        v2 <- unsafeReadMutable v2M
        let _ = [v1,v2,sample] 
        return $ NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort sample) v1 v2
