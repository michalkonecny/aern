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

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Coeff
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

----------------------------------------------------------------
-- Addition

foreign import ccall safe "addUpGenCf"
        poly_addUp :: 
            OpsPtr cf ->
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            IO ()

foreign import ccall safe "addDnGenCf"
        poly_addDn :: 
            OpsPtr cf ->
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            IO ()

foreign import ccall safe "addEnclGenCf"
        poly_addEncl :: 
            OpsPtr cf ->
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            IO ()

polyAddUp ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyAddUp = 
    polyBinaryOp poly_addUp

polyAddDn ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyAddDn = 
    polyBinaryOp poly_addDn

polyAddEncl ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyAddEncl = 
    polyBinaryOp poly_addEncl

----------------------------------------------------------------
-- Multiplication

foreign import ccall safe "multiplyUpGenCf"
        poly_multiplyUp :: 
            OpsPtr cf ->
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            IO ()

foreign import ccall safe "multiplyDnGenCf"
        poly_multiplyDn :: 
            OpsPtr cf ->
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            IO ()

foreign import ccall safe "multiplyEnclGenCf"
        poly_multiplyEncl :: 
            OpsPtr cf ->
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            (Ptr (CPoly cf)) -> 
            IO ()

polyMultiplyUp ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyMultiplyUp = 
    polyBinaryOp poly_multiplyUp

polyMultiplyDn ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyMultiplyDn = 
    polyBinaryOp poly_multiplyDn

polyMultiplyEncl ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyMultiplyEncl = 
    polyBinaryOp poly_multiplyEncl

----------------------------------------------------------------
-- Scaling

foreign import ccall safe "scaleUpThinGenCf"
        poly_scaleUpThin :: 
            OpsPtr cf ->
            (StablePtr (Mutable cf s)) ->
            (Ptr (CPoly cf)) -> 
            IO ()

foreign import ccall safe "scaleDnThinGenCf"
        poly_scaleDnThin :: 
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
    PolyM cf s ->
    ST s ()
polyScaleUp = 
    polyCoeffOp poly_scaleUpThin

polyScaleDn ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf ->
    PolyM cf s ->
    ST s ()
polyScaleDn = 
    polyCoeffOp poly_scaleDnThin

polyScaleEncl ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf ->
    PolyM cf s ->
    ST s ()
polyScaleEncl = 
    polyCoeffOp poly_scaleEncl
