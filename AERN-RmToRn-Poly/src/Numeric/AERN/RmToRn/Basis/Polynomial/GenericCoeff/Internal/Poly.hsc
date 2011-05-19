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

import Foreign.Ptr(Ptr,nullPtr,castPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Marshal.Array (newArray)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import qualified Foreign.Concurrent as Conc (newForeignPtr)

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

{-|
   A dummy pure polynomial with an untyped pointer.
   All operations for this type will be defined by conversions from in-place
   operations on PolyMutable. 
-}
newtype PolyPure cf = PolyPure (ForeignPtr ()) 

instance 
    (CanBeMutable cf, ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) => 
    CanBeMutable (PolyPure cf) 
    where
    newtype Mutable (PolyPure cf) s = PolyMutable (ForeignPtr (Poly (Mutable cf s)))
    unsafeMakeMutable (PolyPure fp) =
        return $ PolyMutable (castForeignPtr fp)
    unsafeReadMutable (PolyMutable fp) =
        return $ PolyPure (castForeignPtr fp)
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
    assignMutable resM@(PolyMutable resFP) srcM@(PolyMutable srcFP) =
        do
        resSizes <- peekSizes resM
        srcSizes <- peekSizes srcM
        case resSizes == srcSizes of
            False -> error "attempt to assign between incompatible polynomial variables"
            True -> return ()
        cM <- peekConst srcM
        let opsPtr = newOpsArithUpDnDefaultEffort (getDummySample cM)
        polyCopySameSizes opsPtr resM srcM
    cloneMutable pM =
        do
        (arity, size, deg) <- peekSizes pM
        resM <- constPoly zero zero arity size deg
        assignMutable resM pM
        return resM
    
data Poly cfm -- defined in poly.c, opaque to Haskell  

type PolyMutable cf s = Mutable (PolyPure cf) s 
    

{-# INLINE peekSizes #-}
peekSizes :: (PolyMutable cf s) -> ST s (Var, Size, Power)
peekSizes p =
    unsafeIOToST $ peekSizesIO p

{-# INLINE peekSizesIO #-}
peekSizesIO :: (PolyMutable cf s) -> IO (Var, Size, Power)
peekSizesIO (PolyMutable fp) =
        withForeignPtr fp $ \ptr -> 
            do
            maxArityC <- #{peek Poly, maxArity} ptr
            maxSizeC <- #{peek Poly, maxSize} ptr
            maxDegreeC <- #{peek Poly, maxDeg} ptr
            return (fromCVar maxArityC, fromCSize maxSizeC, fromCPower maxDegreeC)            

{-# INLINE peekArity #-}
peekArity :: (PolyMutable cf s) -> ST s Var
peekArity p =
    unsafeIOToST $ peekArityIO p
    
{-# INLINE peekArityIO #-}
peekArityIO :: (PolyMutable cf s) -> IO Var
peekArityIO (PolyMutable fp) =
    withForeignPtr fp $ \ptr -> 
        do
        maxArityC <- #{peek Poly, maxArity} ptr
        return (fromCVar maxArityC)

{-# INLINE peekConst #-}
peekConst :: (PolyMutable cf s) -> ST s (Mutable cf s)
peekConst p =
    unsafeIOToST $ peekConstIO p
    
{-# INLINE peekConstIO #-}
peekConstIO :: (PolyMutable cf s) -> IO (Mutable cf s)
peekConstIO (PolyMutable fp) =
    withForeignPtr fp $ \ptr -> 
        do
        constSP <- #{peek Poly, constTerm} ptr
        const <- deRefStablePtr constSP
        return const

{-# INLINE peekError #-}
peekError :: (PolyMutable cf s) -> ST s (Mutable cf s)
peekError p =
    unsafeIOToST $ peekErrorIO p
    
{-# INLINE peekErrorIO #-}
peekErrorIO :: (PolyMutable cf s) -> IO (Mutable cf s)
peekErrorIO (PolyMutable fp) =
    withForeignPtr fp $ \ptr -> 
        do
        errorSP <- #{peek Poly, errorBound} ptr
        error <- deRefStablePtr errorSP
        return error

data Ops s cf =
    Ops
    {
        ops_sample :: {-# UNPACK #-} ! (StablePtr cf),
        ops_new :: {-# UNPACK #-} ! (StablePtr (NewOpMutable s cf)),
        ops_clone :: {-# UNPACK #-} ! (StablePtr (CloneOpMutable s cf)),
        ops_assign :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s cf)),
        ops_assignFromPure :: {-# UNPACK #-} ! (StablePtr (UnaryFromPureOpMutable s cf)),
        ops_negMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s cf)),
        ops_absUpMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s cf)),
        ops_absDnMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s cf)),
        ops_plusUpMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_plusDnMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_minusUpMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_minusDnMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_timesUpMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_timesDnMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf))
    }

instance (Storable cf) => Storable (Ops s cf) where
    sizeOf _ = #size Ops
    alignment _ = #{alignment Ops}
    peek = error "Ops.peek: Not needed and not applicable"
    poke ptr 
            (Ops
              sample new clone assign assignFromPure
              negMutable
              absUpMutable absDnMutable 
              plusUpMutable plusDnMutable 
              minusUpMutable minusDnMutable 
              timesUpMutable timesDnMutable) = 
        do 
        #{poke Ops, sample} ptr sample
        #{poke Ops, new} ptr new
        #{poke Ops, clone} ptr clone
        #{poke Ops, assign} ptr assign
        #{poke Ops, assignFromPure} ptr assignFromPure
        #{poke Ops, negMutable} ptr negMutable
        #{poke Ops, absUpMutable} ptr absUpMutable
        #{poke Ops, absDnMutable} ptr absDnMutable
        #{poke Ops, plusUpMutable} ptr plusUpMutable
        #{poke Ops, plusDnMutable} ptr plusDnMutable
        #{poke Ops, minusUpMutable} ptr minusUpMutable
        #{poke Ops, minusDnMutable} ptr minusDnMutable
        #{poke Ops, timesUpMutable} ptr timesUpMutable
        #{poke Ops, timesDnMutable} ptr timesDnMutable

type (OpsPtr cf) = Ptr ()

newOps ::
    (Storable cf) =>
    (Ops s cf) ->
    IO (OpsPtr cf)
newOps ops =
    do
    opsP <- malloc
    poke opsP ops
    return $ castPtr opsP
    
freeOps :: OpsPtr cf -> IO ()
freeOps = free

mkOps ::
    cf ->
    (NewOpMutable s cf) ->
    (CloneOpMutable s cf) ->
    (UnaryOpMutable s cf) ->
    (UnaryFromPureOpMutable s cf) ->
    (UnaryOpMutable s cf) ->
    (UnaryOpMutable s cf) ->
    (UnaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    IO (Ops s cf)
mkOps sample new clone assign assignFromPure 
        neg absUp absDn addUp addDn subtrUp subtrDn multUp multDn =
    do
    sampleSP <- newStablePtr sample
    newSP <- newStablePtr new
    cloneSP <- newStablePtr clone  
    assignSP <- newStablePtr assign  
    assignFromPureSP <- newStablePtr assignFromPure  
    negSP <- newStablePtr neg  
    absUpSP <- newStablePtr absUp  
    absDnSP <- newStablePtr absDn
    addUpSP <- newStablePtr addUp   
    subtrDnSP <- newStablePtr subtrDn
    subtrUpSP <- newStablePtr subtrUp   
    addDnSP <- newStablePtr addDn
    multUpSP <- newStablePtr multUp   
    multDnSP <- newStablePtr multDn
    return $
      Ops
        sampleSP newSP
        cloneSP assignSP assignFromPureSP
        negSP
        absUpSP absDnSP
        addUpSP addDnSP
        subtrUpSP subtrDnSP
        multUpSP multDnSP

mkOpsArithUpDn ::
    (ArithUpDn.RoundedRealInPlace cf, CanBeMutable cf) => 
    cf ->
    ArithUpDn.RoundedRealEffortIndicator cf -> 
    IO (Ops s cf)
mkOpsArithUpDn sample effort =
    let absEffort = ArithUpDn.rrEffortAbs sample effort in
    let fldEffort = ArithUpDn.rrEffortField sample effort in
    let addEffort = ArithUpDn.fldEffortAdd sample fldEffort in
    let multEffort = ArithUpDn.fldEffortMult sample fldEffort in
    mkOps
        sample
        (makeMutable)
        (cloneMutable)
        (assignMutable)
        (writeMutable)
        (negInPlace)
        (ArithUpDn.absUpInPlaceEff absEffort)
        (ArithUpDn.absDnInPlaceEff absEffort)
        (ArithUpDn.addUpInPlaceEff addEffort)
        (ArithUpDn.addDnInPlaceEff addEffort)
        (ArithUpDn.subtrUpInPlaceEff addEffort)
        (ArithUpDn.subtrDnInPlaceEff addEffort)
        (ArithUpDn.multUpInPlaceEff multEffort)
        (ArithUpDn.multDnInPlaceEff multEffort)

newOpsArithUpDn ::
    (ArithUpDn.RoundedRealInPlace cf, CanBeMutable cf, Storable cf) => 
    cf ->
    ArithUpDn.RoundedRealEffortIndicator cf -> 
    OpsPtr cf
newOpsArithUpDn sample effort =
    unsafePerformIO $
        do
        ops <- mkOpsArithUpDn sample effort
        newOps ops

newOpsArithUpDnDefaultEffort ::
    (ArithUpDn.RoundedRealInPlace cf, CanBeMutable cf, Storable cf) => 
    cf ->
    OpsPtr cf
newOpsArithUpDnDefaultEffort sample =
    newOpsArithUpDn sample (ArithUpDn.roundedRealDefaultEffort sample)



----------------------------------------------------------------

foreign import ccall safe "printPolyGenCf"
        poly_printPoly :: (Ptr (Poly cfm)) -> IO ()  

printPolyInternals :: (PolyMutable cf s) -> IO ()
printPolyInternals (PolyMutable fp) =
    withForeignPtr fp $ \ ptr ->
        poly_printPoly ptr

--------------------------------------------------------------

foreign import ccall safe "freePolyGenCf"
        poly_freePoly :: (Ptr (Poly cfm)) -> IO ()  

concFinalizerFreePoly :: (Ptr (Poly cfm)) -> IO ()
concFinalizerFreePoly p =
    do
--    putStrLn "concFinalizerFreePoly"
    poly_freePoly p

----------------------------------------------------------------
foreign import ccall safe "mapCoeffsInPlaceGenCf"
        poly_mapCoeffsInPlace ::
            (StablePtr (cfm1 -> cfm2)) -> 
            (Ptr (Poly cfm1)) -> 
            IO ()  

----------------------------------------------------------------

foreign import ccall unsafe "newConstPolyGenCf"
        poly_newConstPoly :: 
            (StablePtr cfm) -> 
            (StablePtr cfm) -> 
            CVar -> CSize -> CPower -> 
            IO (Ptr (Poly cfm))  

constPoly :: 
    (Show cf, CanBeMutable cf) => 
    cf -> cf -> 
    Var -> Size -> Power -> 
    ST s (PolyMutable cf s)
constPoly cM radiusM maxArity maxSize maxDeg =
    unsafeIOToST $ constPolyIO cM radiusM maxArity maxSize maxDeg

constPolyIO :: 
    (Show cf, CanBeMutable cf) => 
    cf -> cf -> 
    Var -> Size -> Power -> 
    IO (PolyMutable cf s)
constPolyIO c radius maxArity maxSize maxDeg =
    do
    cM <- unsafeSTToIO $ makeMutable c
    cSP <- newStablePtr cM
    radiusM <- unsafeSTToIO $ makeMutable radius
    radiusSP <- newStablePtr radiusM
--    putStrLn $ "calling newConstPoly for " ++ show c
    pP <- poly_newConstPoly cSP radiusSP (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
--    putStrLn $ "newConstPoly for " ++ show c ++ " returned"
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyMutable fp

----------------------------------------------------------------

foreign import ccall unsafe "newProjectionPolyGenCf"
        poly_newProjectionPoly :: 
            (StablePtr cfm) -> (StablePtr cfm) -> (StablePtr cfm) ->
            CVar -> CVar -> CSize -> CPower -> 
            IO (Ptr (Poly cfm))

projectionPoly :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) 
    => 
    Var -> Var -> Size -> Power -> 
    ST s (PolyMutable cf s)
projectionPoly x maxArity maxSize maxDeg =
    unsafeIOToST $ projectionPolyIO x maxArity maxSize maxDeg


projectionPolyIO :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    Var -> Var -> Size -> Power -> 
    IO (PolyMutable cf s)
projectionPolyIO x maxArity maxSize maxDeg =
    do
    zeroM <- unsafeSTToIO $ makeMutable zero
    zeroSP <- newStablePtr zeroM
    oneM <- unsafeSTToIO $ makeMutable one
    oneSP <- newStablePtr oneM
    radM <- unsafeSTToIO $ makeMutable zero
    radSP <- newStablePtr radM
    pP <- poly_newProjectionPoly zeroSP oneSP radSP (toCVar x) (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyMutable fp

----------------------------------------------------------------

foreign import ccall safe "copySameSizesGenCf"
        poly_copySameSizes :: 
            OpsPtr cf ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

foreign import ccall safe "copyEnclGenCf"
        poly_copyEncl :: 
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            OpsPtr cf ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

foreign import ccall safe "copyUpThinGenCf"
        poly_copyUpThin :: 
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (StablePtr cf) ->
            OpsPtr cf ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

foreign import ccall safe "copyDnThinGenCf"
        poly_copyDnThin :: 
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (StablePtr cf) ->
            OpsPtr cf ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

------------------------------------------------------------------

polyCopySameSizes ::
    (Storable cf, CanBeMutable cf) =>
    OpsPtr cf ->
    PolyMutable cf s ->
    PolyMutable cf s ->
    ST s ()
polyCopySameSizes opsPtr (PolyMutable resFP) (PolyMutable srcFP) =
    do
    unsafeIOToST $
        do
        withForeignPtr resFP $ \resP ->
           withForeignPtr srcFP $ \srcP ->
             poly_copySameSizes opsPtr resP srcP

------------------------------------------------------------------

polyCopyEncl ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    OpsPtr cf ->
    PolyMutable cf s ->
    PolyMutable cf s ->
    ST s ()
polyCopyEncl opsPtr = 
    polyCopyOpMutable poly_copyEncl zero opsPtr

polyCopyOpMutable copyOp sample opsPtr (PolyMutable resFP) (PolyMutable srcFP) =
    do
    unsafeIOToST $
      do
      compareSP <- newStablePtr compareMutable
      _ <- withForeignPtr resFP $ \resP ->
             withForeignPtr srcFP $ \srcP ->
             copyOp compareSP opsPtr resP srcP
      freeStablePtr compareSP
      return ()
    where
    compareMutable v1M v2M =
        unsafePerformIO $ unsafeSTToIO $
        do
        v1 <- unsafeReadMutable v1M
        v2 <- unsafeReadMutable v2M
        let _ = [v1,v2,sample] 
        return $ NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort sample) v1 v2

----------------------------------------------------------------
        
polyCopyUpThin ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf ->
    OpsPtr cf ->
    PolyMutable cf s ->
    PolyMutable cf s ->
    ST s ()
polyCopyUpThin sample opsPtr = 
    polyCopyThinOpMutable poly_copyUpThin zero opsPtr

polyCopyDnThin ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf ->
    OpsPtr cf ->
    PolyMutable cf s ->
    PolyMutable cf s ->
    ST s ()
polyCopyDnThin sample opsPtr = 
    polyCopyThinOpMutable poly_copyDnThin zero opsPtr

polyCopyThinOpMutable copyOp sample opsPtr (PolyMutable resFP) (PolyMutable srcFP) =
    do
    unsafeIOToST $
      do
      zeroSP <- newStablePtr zero
      compareSP <- newStablePtr compareMutable
      _ <- withForeignPtr resFP $ \resP ->
             withForeignPtr srcFP $ \srcP ->
             copyOp compareSP zeroSP opsPtr resP srcP
      freeStablePtr compareSP
      return ()
    where
    compareMutable v1M v2M =
        unsafePerformIO $ unsafeSTToIO $
        do
        v1 <- unsafeReadMutable v1M
        v2 <- unsafeReadMutable v2M
        let _ = [v1,v2,sample] 
        return $ NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort sample) v1 v2
       