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

import Foreign.Ptr(Ptr,nullPtr)
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
   operations on PolyFP. 
-}
newtype PolyPure cf = PolyPure (ForeignPtr ()) 

instance 
    (CanBeMutable cf, ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) => 
    CanBeMutable (PolyPure cf) 
    where
    newtype Mutable (PolyPure cf) s = PolyFP (ForeignPtr (Poly (Mutable cf s)))
    unsafeMakeMutable (PolyPure fp) =
        return $ PolyFP (castForeignPtr fp)
    unsafeReadMutable (PolyFP fp) =
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
    assignMutable resM@(PolyFP resFP) srcM@(PolyFP srcFP) =
        do
        resSizes <- peekSizes resM
        srcSizes <- peekSizes srcM
        case resSizes == srcSizes of
            False -> error "attempt to assign between incompatible polynomial variables"
            True -> return ()
        cM <- peekConst srcM
        let opsMutablePtr = newOpsMutableArithUpDnDefaultEffort (getDummySample cM)
        polyCopySameSizes opsMutablePtr resM srcM
    cloneMutable pM =
        do
        (arity, size, deg) <- peekSizes pM
        resM <- constPoly zero zero arity size deg
        assignMutable resM pM
        return resM
    
data Poly cfm -- defined in poly.c, opaque to Haskell  

type PolyFP cf s = Mutable (PolyPure cf) s 
    

{-# INLINE peekSizes #-}
peekSizes :: (PolyFP cf s) -> ST s (Var, Size, Power)
peekSizes p =
    unsafeIOToST $ peekSizesIO p

{-# INLINE peekSizesIO #-}
peekSizesIO :: (PolyFP cf s) -> IO (Var, Size, Power)
peekSizesIO (PolyFP fp) =
        withForeignPtr fp $ \ptr -> 
            do
            maxArityC <- #{peek Poly, maxArity} ptr
            maxSizeC <- #{peek Poly, maxSize} ptr
            maxDegreeC <- #{peek Poly, maxDeg} ptr
            return (fromCVar maxArityC, fromCSize maxSizeC, fromCPower maxDegreeC)            

{-# INLINE peekArity #-}
peekArity :: (PolyFP cf s) -> ST s Var
peekArity p =
    unsafeIOToST $ peekArityIO p
    
{-# INLINE peekArityIO #-}
peekArityIO :: (PolyFP cf s) -> IO Var
peekArityIO (PolyFP fp) =
    withForeignPtr fp $ \ptr -> 
        do
        maxArityC <- #{peek Poly, maxArity} ptr
        return (fromCVar maxArityC)

{-# INLINE peekConst #-}
peekConst :: (PolyFP cf s) -> ST s (Mutable cf s)
peekConst p =
    unsafeIOToST $ peekConstIO p
    
{-# INLINE peekConstIO #-}
peekConstIO :: (PolyFP cf s) -> IO (Mutable cf s)
peekConstIO (PolyFP fp) =
    withForeignPtr fp $ \ptr -> 
        do
        constSP <- #{peek Poly, constTerm} ptr
        const <- deRefStablePtr constSP
        return const

{-# INLINE peekError #-}
peekError :: (PolyFP cf s) -> ST s (Mutable cf s)
peekError p =
    unsafeIOToST $ peekErrorIO p
    
{-# INLINE peekErrorIO #-}
peekErrorIO :: (PolyFP cf s) -> IO (Mutable cf s)
peekErrorIO (PolyFP fp) =
    withForeignPtr fp $ \ptr -> 
        do
        errorSP <- #{peek Poly, errorBound} ptr
        error <- deRefStablePtr errorSP
        return error

data Ops_Mutable s t =
    Ops_Mutable
    {
        ops_sample :: {-# UNPACK #-} ! (StablePtr t),
        ops_new :: {-# UNPACK #-} ! (StablePtr (NewOpMutable s t)),
        ops_clone :: {-# UNPACK #-} ! (StablePtr (CloneOpMutable s t)),
        ops_assign :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s t)),
        ops_assignFromPure :: {-# UNPACK #-} ! (StablePtr (UnaryFromPureOpMutable s t)),
        ops_negMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s t)),
        ops_absUpMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s t)),
        ops_absDnMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s t)),
        ops_plusUpMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s t)),
        ops_plusDnMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s t)),
        ops_minusUpMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s t)),
        ops_minusDnMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s t)),
        ops_timesUpMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s t)),
        ops_timesDnMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s t))
    }

instance (Storable cf) => Storable (Ops_Mutable s cf) where
    sizeOf _ = #size Ops_Mutable
    alignment _ = #{alignment Ops_Mutable}
    peek = error "Ops_Mutable.peek: Not needed and not applicable"
    poke ptr 
            (Ops_Mutable
              sample new clone assign assignFromPure
              negMutable
              absUpMutable absDnMutable 
              plusUpMutable plusDnMutable 
              minusUpMutable minusDnMutable 
              timesUpMutable timesDnMutable) = 
        do 
        #{poke Ops_Mutable, sample} ptr sample
        #{poke Ops_Mutable, new} ptr new
        #{poke Ops_Mutable, clone} ptr clone
        #{poke Ops_Mutable, assign} ptr assign
        #{poke Ops_Mutable, assignFromPure} ptr assignFromPure
        #{poke Ops_Mutable, negMutable} ptr negMutable
        #{poke Ops_Mutable, absUpMutable} ptr absUpMutable
        #{poke Ops_Mutable, absDnMutable} ptr absDnMutable
        #{poke Ops_Mutable, plusUpMutable} ptr plusUpMutable
        #{poke Ops_Mutable, plusDnMutable} ptr plusDnMutable
        #{poke Ops_Mutable, minusUpMutable} ptr minusUpMutable
        #{poke Ops_Mutable, minusDnMutable} ptr minusDnMutable
        #{poke Ops_Mutable, timesUpMutable} ptr timesUpMutable
        #{poke Ops_Mutable, timesDnMutable} ptr timesDnMutable

newOps_Mutable ::
    (Storable cf) =>
    (Ops_Mutable s cf) ->
    IO (Ptr (Ops_Mutable s cf))
newOps_Mutable ops =
    do
    opsP <- malloc
    poke opsP ops
    return opsP
    
freeOps_Mutable :: (Ptr (Ops_Mutable s cf)) -> IO ()
freeOps_Mutable = free

mkOpsMutable ::
    t ->
    (NewOpMutable s t) ->
    (CloneOpMutable s t) ->
    (UnaryOpMutable s t) ->
    (UnaryFromPureOpMutable s t) ->
    (UnaryOpMutable s t) ->
    (UnaryOpMutable s t) ->
    (UnaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    IO (Ops_Mutable s t)
mkOpsMutable sample new clone assign assignFromPure 
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
      Ops_Mutable
        sampleSP newSP
        cloneSP assignSP assignFromPureSP
        negSP
        absUpSP absDnSP
        addUpSP addDnSP
        subtrUpSP subtrDnSP
        multUpSP multDnSP

mkOpsMutableArithUpDn ::
    (ArithUpDn.RoundedRealInPlace t, CanBeMutable t) => 
    t ->
    ArithUpDn.RoundedRealEffortIndicator t -> 
    IO (Ops_Mutable s t)
mkOpsMutableArithUpDn sample effort =
    let absEffort = ArithUpDn.rrEffortAbs sample effort in
    let fldEffort = ArithUpDn.rrEffortField sample effort in
    let addEffort = ArithUpDn.fldEffortAdd sample fldEffort in
    let multEffort = ArithUpDn.fldEffortMult sample fldEffort in
    mkOpsMutable
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

newOpsMutableArithUpDn ::
    (ArithUpDn.RoundedRealInPlace t, CanBeMutable t, Storable t) => 
    t ->
    ArithUpDn.RoundedRealEffortIndicator t -> 
    Ptr (Ops_Mutable s t)
newOpsMutableArithUpDn sample effort =
    unsafePerformIO $
        do
        ops <- mkOpsMutableArithUpDn sample effort
        newOps_Mutable ops

newOpsMutableArithUpDnDefaultEffort ::
    (ArithUpDn.RoundedRealInPlace t, CanBeMutable t, Storable t) => 
    t ->
    Ptr (Ops_Mutable s t)
newOpsMutableArithUpDnDefaultEffort sample =
    newOpsMutableArithUpDn sample (ArithUpDn.roundedRealDefaultEffort sample)

----------------------------------------------------------------

foreign import ccall safe "printPolyGenCf"
        poly_printPoly :: (Ptr (Poly cfm)) -> IO ()  

printPolyInternals :: (PolyFP cf s) -> IO ()
printPolyInternals (PolyFP fp) =
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
    ST s (PolyFP cf s)
constPoly cM radiusM maxArity maxSize maxDeg =
    unsafeIOToST $ constPolyIO cM radiusM maxArity maxSize maxDeg

constPolyIO :: 
    (Show cf, CanBeMutable cf) => 
    cf -> cf -> 
    Var -> Size -> Power -> 
    IO (PolyFP cf s)
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
    return $ PolyFP fp

----------------------------------------------------------------

foreign import ccall unsafe "newProjectionPolyGenCf"
        poly_newProjectionPoly :: 
            (StablePtr cfm) -> (StablePtr cfm) -> (StablePtr cfm) ->
            CVar -> CVar -> CSize -> CPower -> 
            IO (Ptr (Poly cfm))

projectionPoly :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    Var -> Var -> Size -> Power -> ST s (PolyFP cf s)
projectionPoly x maxArity maxSize maxDeg =
    unsafeIOToST $ projectionPolyIO x maxArity maxSize maxDeg


projectionPolyIO :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    Var -> Var -> Size -> Power -> IO (PolyFP cf s)
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
    return $ PolyFP fp

----------------------------------------------------------------

foreign import ccall safe "copySameSizesGenCf"
        poly_copySameSizes :: 
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

foreign import ccall safe "copyEnclGenCf"
        poly_copyEncl :: 
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

foreign import ccall safe "copyUpThinGenCf"
        poly_copyUpThin :: 
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (StablePtr cf) ->
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

foreign import ccall safe "copyDnThinGenCf"
        poly_copyDnThin :: 
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (StablePtr cf) ->
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

------------------------------------------------------------------

polyCopySameSizes ::
    (Storable cf, CanBeMutable cf) =>
    Ptr (Ops_Mutable s cf) ->
    PolyFP cf s ->
    PolyFP cf s ->
    ST s ()
polyCopySameSizes opsMutablePtr (PolyFP resFP) (PolyFP srcFP) =
    do
    unsafeIOToST $
        do
        withForeignPtr resFP $ \resP ->
           withForeignPtr srcFP $ \srcP ->
             poly_copySameSizes opsMutablePtr resP srcP

------------------------------------------------------------------

polyCopyEncl ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    (Ptr (Ops_Mutable s cf)) ->
    PolyFP cf s ->
    PolyFP cf s ->
    ST s ()
polyCopyEncl opsMutablePtr = 
    polyCopyOpMutable poly_copyEncl sample opsMutablePtr
    where
    sample = zero
    _ = 
        do
        opsMutable <- peek opsMutablePtr
        sample2 <- deRefStablePtr (ops_sample opsMutable)
        return [sample,  sample2]

polyCopyOpMutable copyOp sample opsMutablePtr (PolyFP resFP) (PolyFP srcFP) =
    do
    unsafeIOToST $
      do
      compareSP <- newStablePtr compareMutable
      _ <- withForeignPtr resFP $ \resP ->
             withForeignPtr srcFP $ \srcP ->
             copyOp compareSP opsMutablePtr resP srcP
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
    (Ptr (Ops_Mutable s cf)) ->
    PolyFP cf s ->
    PolyFP cf s ->
    ST s ()
polyCopyUpThin sample opsMutablePtr = 
    polyCopyThinOpMutable poly_copyUpThin sample opsMutablePtr
    where
    sample = zero
    _ = 
        do
        opsMutable <- peek opsMutablePtr
        sample2 <- deRefStablePtr (ops_sample opsMutable)
        return [sample,  sample2]

polyCopyDnThin ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf ->
    (Ptr (Ops_Mutable s cf)) ->
    PolyFP cf s ->
    PolyFP cf s ->
    ST s ()
polyCopyDnThin sample opsMutablePtr = 
    polyCopyThinOpMutable poly_copyDnThin sample opsMutablePtr
    where
    sample = zero
    _ = 
        do
        opsMutable <- peek opsMutablePtr
        sample2 <- deRefStablePtr (ops_sample opsMutable)
        return [sample,  sample2]

polyCopyThinOpMutable copyOp sample opsMutablePtr (PolyFP resFP) (PolyFP srcFP) =
    do
    unsafeIOToST $
      do
      zeroSP <- newStablePtr zero -- $ head [zero, sample]
      compareSP <- newStablePtr compareMutable
      _ <- withForeignPtr resFP $ \resP ->
             withForeignPtr srcFP $ \srcP ->
             copyOp compareSP zeroSP opsMutablePtr resP srcP
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
       