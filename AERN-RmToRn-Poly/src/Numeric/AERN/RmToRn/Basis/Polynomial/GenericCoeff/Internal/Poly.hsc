{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
#include <GenericCoeff/poly.h>
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly
    Description :  Haskell interface to C polynomials with Haskell coefficients
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Haskell interface to C polynomials with Haskell coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly 
(
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Poly,
    module Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Coeff 

import Numeric.AERN.Misc.Debug

import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Data.Word

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST, runST, unsafeIOToST, unsafeSTToIO)

import System.IO.Unsafe

import Foreign.Ptr(Ptr,nullPtr,castPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Marshal.Array (newArray)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import qualified Foreign.Concurrent as Conc (newForeignPtr)

{-|
   A dummy pure polynomial with an untyped pointer.
   All operations for this type will be defined by conversions from in-place
   operations on PolyMutable. 
-}
data Poly cf = 
    Poly 
        {
            poly_ops :: OpsFP cf, -- coefficient operations to pass to some C functions 
            poly_poly :: PolyFP cf -- a polynomial structure
        }
        
type PolyFP cf = ForeignPtr (CPoly cf)
data CPoly cf -- transparent to Haskell - details available in C

instance 
    (CanBeMutable cf, ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) => 
    CanBeMutable (Poly cf) 
    where
    newtype Mutable (Poly cf) s = PolyM (Poly cf)
    unsafeMakeMutable poly = return $ PolyM poly
    unsafeReadMutable (PolyM poly) = return poly
    makeMutable p =
        do
        pM <- unsafeMakeMutable p
        cloneMutable pM
    readMutable pM =
        do
        resM <- cloneMutable pM
        unsafeReadMutable resM    
    writeMutable resM src =
        do
        srcM <- unsafeMakeMutable src
        assignMutable resM srcM
    unsafeWriteMutable = writeMutable 
    assignMutable resM srcM =
        do
        resSizes <- peekSizesM resM
        srcSizes <- peekSizesM srcM
        case resSizes == srcSizes of
            False -> error "attempt to assign between incompatible polynomial variables"
            True -> return ()
        polyCopySameSizes resM srcM
    cloneMutable pM@(PolyM (Poly opsFP _)) =
        do
        (arity, size, deg, tarity) <- peekSizesM pM
        resM <- constPolyM opsFP arity size deg tarity zero zero
        assignMutable resM pM
        return resM
    sameVariable p1M@(PolyM (Poly _ fp1)) p2M@(PolyM (Poly _ fp2)) =
        fp1 == fp2
type PolyM cf s = Mutable (Poly cf) s

{-# INLINE peekSizes #-}
peekSizes :: (Poly cf) -> (Var, Size, Power, Var)
peekSizes p =
    unsafePerformIO $ peekSizesIO p

{-# INLINE peekSizesM #-}
peekSizesM :: (PolyM cf s) -> ST s (Var, Size, Power, Var)
peekSizesM (PolyM p) =
    unsafeIOToST $ peekSizesIO p

{-# INLINE peekSizesIO #-}
peekSizesIO :: (Poly cf) -> IO (Var, Size, Power, Var)
peekSizesIO (Poly opsFP polyFP) =
    withForeignPtr polyFP $ \ptr -> 
        do
        maxArityC <- #{peek Poly, maxArity} ptr
        maxSizeC <- #{peek Poly, maxSize} ptr
        maxDegreeC <- #{peek Poly, maxDeg} ptr
        maxTermArityC <- #{peek Poly, maxTermArity} ptr
        return (fromCVar maxArityC, fromCSize maxSizeC, 
                fromCPower maxDegreeC, fromCVar maxTermArityC)            

{-# INLINE peekArity #-}
peekArity :: (Poly cf) -> Var
peekArity p =
    unsafePerformIO $ peekArityIO p
    
{-# INLINE peekArityM #-}
peekArityM :: (PolyM cf s) -> ST s Var
peekArityM (PolyM p) =
    unsafeIOToST $ peekArityIO p
    
{-# INLINE peekArityIO #-}
peekArityIO :: (Poly cf) -> IO Var
peekArityIO (Poly _ polyFP) =
    withForeignPtr polyFP $ \ptr -> 
        do
        maxArityC <- #{peek Poly, maxArity} ptr
        return (fromCVar maxArityC)

{-# INLINE peekConst #-}
peekConst :: 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    =>
    (Poly cf) -> cf
peekConst p =
    runST $
        do
        pM <- unsafeMakeMutable p 
        cM <- peekConstM pM
        readMutable cM
    
{-# INLINE peekConstM #-}
peekConstM :: (PolyM cf s) -> ST s (Mutable cf s)
peekConstM pM =
    unsafeIOToST $ peekConstIO pM
    
{-# INLINE peekConstIO #-}
peekConstIO :: (PolyM cf s) -> IO (Mutable cf s)
peekConstIO (PolyM (Poly _ polyFP)) =
    withForeignPtr polyFP $ \ptr -> 
        do
        constSP <- #{peek Poly, constTerm} ptr
        cM <- deRefStablePtr constSP
        return cM

{-# INLINE peekError #-}
peekError :: 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    =>
    (Poly cf) -> cf
peekError p =
    runST $
        do
        pM <- unsafeMakeMutable p 
        eM <- peekErrorM pM
        readMutable eM

{-# INLINE peekErrorM #-}
peekErrorM :: (PolyM cf s) -> ST s (Mutable cf s)
peekErrorM pM =
    unsafeIOToST $ peekErrorIO pM
    
{-# INLINE peekErrorIO #-}
peekErrorIO :: (PolyM cf s) -> IO (Mutable cf s)
peekErrorIO (PolyM (Poly _ polyFP)) =
    withForeignPtr polyFP $ \ptr -> 
        do
        errorSP <- #{peek Poly, errorBound} ptr
        eM <- deRefStablePtr errorSP
        return eM

----------------------------------------------------------------

foreign import ccall safe "printPolyGenCf"
        poly_printPoly :: (Ptr (CPoly cfm)) -> IO ()  

printPolyInternalsM :: (PolyM cf s) -> IO ()
printPolyInternalsM (PolyM (Poly _ polyFP)) =
    withForeignPtr polyFP $ \ ptr ->
        poly_printPoly ptr

printPolyInternals :: (Poly cf) -> IO ()
printPolyInternals (Poly _ polyFP) =
    withForeignPtr polyFP $ \ ptr ->
        poly_printPoly ptr

--------------------------------------------------------------

foreign import ccall safe "freePolyGenCf"
        poly_freePoly :: (Ptr (CPoly cfm)) -> IO ()  

concFinalizerFreePoly :: (Ptr (CPoly cfm)) -> IO ()
concFinalizerFreePoly p =
    do
--    putStrLn "concFinalizerFreePoly"
    poly_freePoly p

----------------------------------------------------------------
foreign import ccall safe "mapCoeffsInPlaceGenCf"
        poly_mapCoeffsInPlace ::
            (StablePtr (cfm1 -> cfm2)) -> 
            (Ptr (CPoly cfm1)) -> 
            IO ()  

----------------------------------------------------------------

foreign import ccall unsafe "newConstPolyGenCf"
        poly_newConstPoly :: 
            (StablePtr (Mutable cf s)) -> 
            (StablePtr (Mutable cf s)) -> 
            CVar -> CSize -> CPower -> CVar -> 
            IO (Ptr (CPoly cf))  

constPoly :: 
    (Show cf, CanBeMutable cf) => 
    OpsFP cf -> 
    Var -> Size -> Power -> Var ->
    cf -> cf -> 
    Poly cf
constPoly opsFP maxArity maxSize maxDeg maxTermArity c radius =
    unsafePerformIO $ constPolyIO opsFP maxArity maxSize maxDeg maxTermArity c radius

constPolyM :: 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    =>
    OpsFP cf -> 
    Var -> Size -> Power -> Var ->
    cf -> cf -> 
    ST s (PolyM cf s)
constPolyM opsFP maxArity maxSize maxDeg maxTermArity c radius =
    do
    p <- unsafeIOToST $ constPolyIO opsFP maxArity maxSize maxDeg maxTermArity c radius
    unsafeMakeMutable p

constPolyIO :: 
    (Show cf, CanBeMutable cf) => 
    OpsFP cf -> 
    Var -> Size -> Power -> Var ->
    cf -> cf -> 
    IO (Poly cf)
constPolyIO opsFP maxArity maxSize maxDeg maxTermArity c radius =
    do
    cM <- unsafeSTToIO $ makeMutable c
    cSP <- newStablePtr cM
    radiusM <- unsafeSTToIO $ makeMutable radius
    radiusSP <- newStablePtr radiusM
--    putStrLn $ "calling newConstPoly for " ++ show c
    pP <- poly_newConstPoly cSP radiusSP 
            (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg) (toCVar maxTermArity)
--    putStrLn $ "newConstPoly for " ++ show c ++ " returned"
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ Poly opsFP fp 

----------------------------------------------------------------

foreign import ccall unsafe "newProjectionPolyGenCf"
        poly_newProjectionPoly :: 
            (StablePtr (Mutable cf s)) -> 
            (StablePtr (Mutable cf s)) -> 
            (StablePtr (Mutable cf s)) -> 
            CVar -> CVar -> CSize -> CPower -> CVar -> 
            IO (Ptr (CPoly cf))

projectionPoly :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) 
    => 
    OpsFP cf -> 
    Var -> Size -> Power -> Var ->
    Var -> 
    Poly cf
projectionPoly opsFP maxArity maxSize maxDeg maxTermArity x =
    unsafePerformIO $ projectionPolyIO opsFP maxArity maxSize maxDeg maxTermArity x

projectionPolyM :: 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    =>
    OpsFP cf -> 
    Var -> Size -> Power -> Var ->
    Var -> 
    ST s (PolyM cf s)
projectionPolyM opsFP maxArity maxSize maxDeg maxTermArity x =
    do
    p <- unsafeIOToST $ projectionPolyIO opsFP maxArity maxSize maxDeg maxTermArity x
    unsafeMakeMutable p

projectionPolyIO :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    OpsFP cf -> 
    Var -> Size -> Power -> Var -> 
    Var -> 
    IO (Poly cf)
projectionPolyIO opsFP maxArity maxSize maxDeg maxTermArity x =
    do
    zeroM <- unsafeSTToIO $ makeMutable zero
    zeroSP <- newStablePtr zeroM
    oneM <- unsafeSTToIO $ makeMutable one
    oneSP <- newStablePtr oneM
    radM <- unsafeSTToIO $ makeMutable zero
    radSP <- newStablePtr radM
    pP <- poly_newProjectionPoly zeroSP oneSP radSP 
            (toCVar x) (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg) (toCVar maxTermArity)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ Poly opsFP fp

----------------------------------------------------------------

foreign import ccall unsafe "newTestPolyGenCf"
        poly_newTestPoly :: 
            (StablePtr (Mutable cf s)) -> 
            (StablePtr (Mutable cf s)) -> 
            (StablePtr (Mutable cf s)) -> 
            (StablePtr (Mutable cf s)) -> 
            IO (Ptr (CPoly cf))  

testPoly :: 
    (Show cf, CanBeMutable cf) => 
    OpsFP cf -> 
    cf -> cf -> cf -> cf -> 
    Poly cf
testPoly opsFP c a0 a1 radius =
    unsafePerformIO $ testPolyIO opsFP c a0 a1 radius

testPolyM :: 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    =>
    OpsFP cf -> 
    cf -> cf -> cf -> cf -> 
    ST s (PolyM cf s)
testPolyM opsFP c a0 a1 radius =
    do
    p <- unsafeIOToST $ testPolyIO opsFP c a0 a1 radius
    unsafeMakeMutable p

testPolyIO :: 
    (Show cf, CanBeMutable cf) => 
    OpsFP cf -> 
    cf -> cf -> cf -> cf -> 
    IO (Poly cf)
testPolyIO opsFP c a0 a1 radius =
    do
    cM <- unsafeSTToIO $ makeMutable c
    cSP <- newStablePtr cM
    a0M <- unsafeSTToIO $ makeMutable a0
    a0SP <- newStablePtr a0M
    a1M <- unsafeSTToIO $ makeMutable a1
    a1SP <- newStablePtr a1M
    radiusM <- unsafeSTToIO $ makeMutable radius
    radiusSP <- newStablePtr radiusM
--    putStrLn $ "calling newTestPoly for " ++ show c
    pP <- poly_newTestPoly cSP a0SP a1SP radiusSP 
--    putStrLn $ "newTestPoly for " ++ show c ++ " returned"
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ Poly opsFP fp 

----------------------------------------------------------------

foreign import ccall safe "copySameSizesGenCf"
        poly_copySameSizes :: 
            OpsPtr cf ->
            (Ptr (CPoly cfm)) -> 
            (Ptr (CPoly cfm)) -> 
            IO ()

foreign import ccall safe "copyEnclGenCf"
        poly_copyEncl :: 
            OpsPtr cf ->
            (Ptr (CPoly cfm)) -> 
            (Ptr (CPoly cfm)) -> 
            IO ()

foreign import ccall safe "copyUpThinGenCf"
        poly_copyUpThin :: 
            OpsPtr cf ->
            (Ptr (CPoly cfm)) -> 
            (Ptr (CPoly cfm)) -> 
            IO ()

foreign import ccall safe "copyDnThinGenCf"
        poly_copyDnThin :: 
            OpsPtr cf ->
            (Ptr (CPoly cfm)) -> 
            (Ptr (CPoly cfm)) -> 
            IO ()

------------------------------------------------------------------

polyCopySameSizes ::
    (Storable cf, CanBeMutable cf) =>
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyCopySameSizes = polyUnaryOp poly_copySameSizes

polyCopyEncl ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyCopyEncl = polyUnaryOp poly_copyEncl

polyCopyUpThin ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyCopyUpThin = polyUnaryOp poly_copyUpThin

polyCopyDnThin ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyCopyDnThin = polyUnaryOp poly_copyDnThin 

----------------------------------------------------------------
        
polyUnaryOp unaryOp 
        (PolyM (Poly opsFP resFP)) 
        (PolyM (Poly _ srcFP)) 
    =
    unsafeIOToST $
      withForeignPtr resFP $ \resP ->
        withForeignPtr srcFP $ \srcP ->
          withForeignPtr opsFP $ \opsP ->
            unaryOp opsP resP srcP
        
polyBinaryOp binaryOp
        (PolyM (Poly opsFP resFP))
        (PolyM (Poly _ p1FP))
        (PolyM (Poly _ p2FP)) 
    =
    unsafeIOToST $
      withForeignPtr resFP $ \resP ->
        withForeignPtr p1FP $ \p1P ->
          withForeignPtr p2FP $ \p2P ->
            withForeignPtr opsFP $ \opsP ->
              binaryOp opsP resP p1P p2P
       
polyCoeffOp coeffOp 
        coeff 
        (PolyM (Poly opsFP resAndSrcFP)) 
    =
    do
    coeffM <- unsafeMakeMutable coeff
    unsafeIOToST $
        do
        coeffSP <- newStablePtr coeffM
        withForeignPtr resAndSrcFP $ \resAndSrcP ->
          withForeignPtr opsFP $ \opsP ->
            coeffOp opsP coeffSP resAndSrcP
        freeStablePtr coeffSP

       
       