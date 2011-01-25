{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
#include <DoubleCoeff/poly.h>
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly
    Description :  Haskell interface to C polynomials with native Double coefficients
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Haskell interface to C polynomials with native Double coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly
(
    module Numeric.AERN.RmToRn.Basis.Polynomial.DoubleCoeff.Poly,
    module Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics
)
where

import Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Foreign.C.Types (CDouble)
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
    
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Poly -- defined in poly.c, opaque to Haskell  

newtype PolyFP = PolyFP (ForeignPtr (Poly))

{-# INLINE peekSizes #-}
peekSizes :: (PolyFP) -> (Var, Size)
peekSizes p =
    unsafePerformIO $ peekSizesIO p

{-# INLINE peekSizesIO #-}
peekSizesIO :: (PolyFP) -> IO (Var, Size)
peekSizesIO (PolyFP fp) =
        withForeignPtr fp $ \ptr -> 
            do
            maxArityC <- #{peek Poly, maxArity} ptr
            maxSizeC <- #{peek Poly, maxSize} ptr
            return (fromCVar maxArityC, fromCSize maxSizeC)            

{-# INLINE peekArity #-}
peekArity :: (PolyFP) -> Var
peekArity p =
    unsafePerformIO $ peekArityIO p
    
{-# INLINE peekArityIO #-}
peekArityIO :: (PolyFP) -> IO Var
peekArityIO (PolyFP fp) =
    withForeignPtr fp $ \ptr -> 
        do
        maxArityC <- #{peek Poly, maxArity} ptr
        return (fromCVar maxArityC)

{-# INLINE peekConst #-}
peekConst :: (PolyFP) -> Double
peekConst p =
    unsafePerformIO $ peekConstIO p
    
{-# INLINE peekConstIO #-}
peekConstIO :: (PolyFP) -> IO Double
peekConstIO (PolyFP fp) =
    withForeignPtr fp $ \ptr -> 
        do
        constC <- #{peek Poly, constTerm} ptr
        return $ cDouble2Double constC

data Ops_Pure = Ops_Pure

instance Storable (Ops_Pure) where
    sizeOf _ = #size Ops_Pure
    alignment _ = #{alignment Ops_Pure}
    peek = error "Ops_Pure.peek: Not needed and not applicable"
    poke ptr Ops_Pure = 
        do
        return () 

newOps ::
    (Ops_Pure) -> IO (Ptr (Ops_Pure))
newOps ops =
    do
    opsP <- malloc
    poke opsP ops
    return opsP
    
freeOps :: (Ptr (Ops_Pure)) -> IO ()
freeOps = free

mkOpsPure ::
    IO (Ops_Pure)
mkOpsPure =
    do
    return $
      Ops_Pure

----------------------------------------------------------------

foreign import ccall safe "freePolyDblCf"
        poly_freePoly :: (Ptr (Poly)) -> IO ()  

concFinalizerFreePoly :: (Ptr (Poly)) -> IO ()
concFinalizerFreePoly = poly_freePoly

----------------------------------------------------------------

foreign import ccall unsafe "newConstPolyDblCf"
        poly_newConstPoly :: 
            CDouble -> 
            CDouble ->
            CVar -> CSize -> CPower -> 
            IO (Ptr (Poly))  

constPoly :: Double -> Double -> Var -> Size -> Power -> (PolyFP)
constPoly c radius maxArity maxSize maxDeg =
    unsafePerformIO $ newConstPoly c radius maxArity maxSize maxDeg

newConstPoly :: Double -> Double -> Var -> Size -> Power -> IO (PolyFP)
newConstPoly c radius maxArity maxSize maxDeg =
    do
    let cC = double2CDouble c
    let radiusC = double2CDouble radius
--    putStrLn $ "calling newConstPoly for " ++ show c
    pP <- poly_newConstPoly cC radiusC (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
--    putStrLn $ "newConstPoly for " ++ show c ++ " returned"
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyFP fp

----------------------------------------------------------------

foreign import ccall unsafe "newProjectionPolyDblCf"
        poly_newProjectionPoly :: 
            CDouble -> CDouble -> CDouble -> 
            CVar -> CVar -> CSize -> CPower ->
            IO (Ptr (Poly))  

projectionPoly :: 
    Var -> Var -> Size -> Power -> (PolyFP)
projectionPoly x maxArity maxSize maxDeg =
    unsafePerformIO $ newProjectionPoly x maxArity maxSize maxDeg

newProjectionPoly :: 
    Var -> Var -> Size -> Power -> IO (PolyFP)
newProjectionPoly x maxArity maxSize maxDeg =
    do
    pP <- poly_newProjectionPoly 0 1 0 (toCVar x) (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyFP fp

----------------------------------------------------------------

foreign import ccall safe "addUpUsingPureOpsDblCf"
        poly_addUpUsingPureOps :: 
            CDouble ->
            (StablePtr ()) ->
            (Ptr (Ops_Pure)) ->
            (Ptr (Poly)) -> 
            (Ptr (Poly)) -> 
            (Ptr (Poly)) -> 
            IO ()

foreign import ccall safe "addDnUsingPureOpsDblCf"
        poly_addDnUsingPureOps :: 
            CDouble ->
            (StablePtr ()) ->
            (Ptr (Ops_Pure)) ->
            (Ptr (Poly)) -> 
            (Ptr (Poly)) -> 
            (Ptr (Poly)) -> 
            IO ()

polyAddUpPureUsingPureOps ::
    Size ->
    Power ->
    (Ptr Ops_Pure) ->
    PolyFP ->
    PolyFP ->
    PolyFP
polyAddUpPureUsingPureOps size deg opsPtr = 
    polyBinaryOpPure poly_addUpUsingPureOps size deg opsPtr

polyAddDnPureUsingPureOps ::
    Size ->
    Power ->
    (Ptr (Ops_Pure)) ->
    PolyFP ->
    PolyFP ->
    PolyFP
polyAddDnPureUsingPureOps size deg opsPtr = 
    polyBinaryOpPure poly_addDnUsingPureOps size deg opsPtr

polyBinaryOpPure binaryOp maxSize maxDeg opsPtr p1@(PolyFP p1FP) (PolyFP p2FP) =
    unsafePerformIO $
    do
    maxArity <- peekArityIO p1
    resP <- poly_newConstPoly 0 0 (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
    compareSP <- newStablePtr $ ()
    _ <- withForeignPtr p1FP $ \p1 ->
             withForeignPtr p2FP $ \p2 ->
                 binaryOp 0 compareSP opsPtr resP p1 p2
    freeStablePtr compareSP
    resFP <- Conc.newForeignPtr resP (concFinalizerFreePoly resP)
    return $ PolyFP resFP

----------------------------------------------------------------

foreign import ccall safe "evalAtPtChebBasisDblCf"
        poly_evalAtPtChebBasis :: 
            (Ptr (Poly)) -> 
            (Ptr (StablePtr val)) -> 
            (StablePtr val) ->
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) ->
            (StablePtr (ConvertFromDoubleOp val)) ->
            IO (StablePtr val)  

evalAtPtChebBasis :: 
    (PolyFP) ->
    [val] {-^ values to substitute for variables @[0..(maxArity-1)]@ -} ->
    val {-^ number @1@ -} ->
    (BinaryOp val) {-^ addition -} -> 
    (BinaryOp val) {-^ subtraction -} -> 
    (BinaryOp val) {-^ multiplication -} -> 
    (ConvertFromDoubleOp val) ->
    val
evalAtPtChebBasis (PolyFP polyFP) vals one add subtr mult cf2val =
    unsafePerformIO $
    do
    valSPs <- mapM newStablePtr vals
    valSPsPtr <- newArray valSPs
    oneSP <- newStablePtr one
    addSP <- newStablePtr add
    subtrSP <- newStablePtr subtr
    multSP <- newStablePtr mult
    cf2valSP <- newStablePtr cf2val
    resSP <- withForeignPtr polyFP $ \polyPtr ->
        poly_evalAtPtChebBasis polyPtr valSPsPtr oneSP addSP subtrSP multSP cf2valSP
    freeStablePtr oneSP
    _ <- mapM freeStablePtr [addSP, subtrSP, multSP]
    free valSPsPtr
    _ <- mapM freeStablePtr valSPs
    res <- deRefStablePtr resSP 
    freeStablePtr resSP
    return res


foreign import ccall safe "evalAtPtPowerBasisDblCf"
        poly_evalAtPtPowerBasis :: 
            (Ptr (Poly)) -> 
            (Ptr (StablePtr val)) -> 
            (StablePtr val) ->
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) ->
            (StablePtr (ConvertFromDoubleOp val)) ->
            IO (StablePtr val)  

evalAtPtPowerBasis :: 
    (PolyFP) ->
    [val] {-^ values to substitute for variables @[0..(maxArity-1)]@ -} ->
    val {-^ number @1@ -} ->
    (BinaryOp val) {-^ addition -} -> 
    (BinaryOp val) {-^ multiplication -} -> 
    (ConvertFromDoubleOp val) ->
    val
evalAtPtPowerBasis (PolyFP polyFP) vals one add mult cf2val =
    unsafePerformIO $
    do
    valSPs <- mapM newStablePtr vals
    valSPsPtr <- newArray valSPs
    oneSP <- newStablePtr one
    addSP <- newStablePtr add
    multSP <- newStablePtr mult
    cf2valSP <- newStablePtr cf2val
    resSP <- withForeignPtr polyFP $ \polyPtr ->
        poly_evalAtPtPowerBasis polyPtr valSPsPtr oneSP addSP multSP cf2valSP
    freeStablePtr oneSP
    _ <- mapM freeStablePtr [addSP, multSP]
    free valSPsPtr
    _ <- mapM freeStablePtr valSPs
    res <- deRefStablePtr resSP 
    freeStablePtr resSP
    return res

    