{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
#include <GenericCoeff/poly.h>
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.RingOps
    Description :  +,* for C polynomials with Haskell coefficients
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Addition and multiplications for C polynomials with Haskell coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.RingOps
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
            OpsPtr cf ->
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            IO ()

foreign import ccall safe "addDnGenCf"
        poly_addDn :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            OpsPtr cf ->
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            IO ()

polyAddUp ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    PolyM cf s ->
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyAddUp sample = 
    polyBinaryOp poly_addUp sample

polyAddDn ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    PolyM cf s ->
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyAddDn sample = 
    polyBinaryOp poly_addDn sample

----------------------------------------------------------------

foreign import ccall safe "scaleUpThinGenCf"
        poly_scaleUpThin :: 
            (StablePtr cf) ->
            OpsPtr cf ->
            (StablePtr (Mutable cf s)) ->
            (Ptr (CPoly cf)) -> 
            IO ()

foreign import ccall safe "scaleDnThinGenCf"
        poly_scaleDnThin :: 
            (StablePtr cf) ->
            OpsPtr cf ->
            (StablePtr (Mutable cf s)) ->
            (Ptr (CPoly cf)) -> 
            IO ()

foreign import ccall safe "scaleEnclGenCf"
        poly_scaleEncl :: 
            OpsPtr cf ->
            (StablePtr (Mutable cf s)) ->
            (Ptr (CPoly cf)) -> 
            IO ()

polyScaleUp ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    cf ->
    PolyM cf s ->
    ST s ()
polyScaleUp sample = 
    polyScalingOp poly_scaleUpThin sample

polyScaleDn ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    cf ->
    PolyM cf s ->
    ST s ()
polyScaleDn sample = 
    polyScalingOp poly_scaleDnThin sample

polyScaleEncl ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf ->
    PolyM cf s ->
    ST s ()
polyScaleEncl = 
    polyScalingOpNoZero poly_scaleEncl

----------------------------------------------------------------

polyScalingOp scalingOp sample scalingFactor (PolyM (Poly opsFP pFP)) =
    do
    sfM <- unsafeMakeMutable scalingFactor 
    unsafeIOToST $
      do
      zeroSP <- newStablePtr zero -- head [zero, sample]
      factorSP <- newStablePtr sfM
      _ <- withForeignPtr pFP $ \pP ->
             withForeignPtr opsFP $ \opsP ->
               scalingOp zeroSP opsP factorSP pP
      return ()

----------------------------------------------------------------

polyScalingOpNoZero scalingOp scalingFactor (PolyM (Poly opsFP pFP)) =
    do
    sfM <- unsafeMakeMutable scalingFactor 
    unsafeIOToST $
      do
      factorSP <- newStablePtr sfM
      _ <- withForeignPtr pFP $ \pP ->
             withForeignPtr opsFP $ \opsP ->
               scalingOp opsP factorSP pP
      return ()

--------------------------------------------------------------

polyBinaryOp binaryOp sample
        (PolyM (Poly opsFP resFP)) (PolyM (Poly _ p1FP)) (PolyM (Poly _ p2FP)) =
    unsafeIOToST $
    do
    zeroSP <- newStablePtr zero
    compareSP <- newStablePtr compareMutable
    _ <- withForeignPtr p1FP $ \p1P ->
           withForeignPtr p2FP $ \p2P ->
             withForeignPtr resFP $ \resP ->
               withForeignPtr opsFP $ \opsP ->
                 binaryOp zeroSP compareSP opsP resP p1P p2P
    freeStablePtr compareSP
    where
    compareMutable v1M v2M =
        unsafePerformIO $ unsafeSTToIO $
        do
        v1 <- unsafeReadMutable v1M
        v2 <- unsafeReadMutable v2M
        let _ = [v1,v2,sample] 
        return $ NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort sample) v1 v2
