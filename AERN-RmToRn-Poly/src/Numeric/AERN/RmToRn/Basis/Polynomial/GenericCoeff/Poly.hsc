{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
#include <GenericCoeff/poly.h>
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly
    Description :  Haskell interface to C polynomials with Haskell coefficients
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Haskell interface to C polynomials with Haskell coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly 
(
    module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly,
    module Numeric.AERN.RmToRn.Basis.Polynomial.Internal.Basics
)
where

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
    
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Poly cf -- defined in poly.c, opaque to Haskell  

newtype PolyFP cf = PolyFP (ForeignPtr (Poly cf))
newtype PolyMutableFP cf s = PolyMutableFP (ForeignPtr (Poly (Mutable cf s)))

{-# INLINE peekSizes #-}
peekSizes :: (PolyFP cf) -> (Var, Size, Power)
peekSizes p =
    unsafePerformIO $ peekSizesIO p

{-# INLINE peekSizesIO #-}
peekSizesIO :: (PolyFP cf) -> IO (Var, Size, Power)
peekSizesIO (PolyFP fp) =
        withForeignPtr fp $ \ptr -> 
            do
            maxArityC <- #{peek Poly, maxArity} ptr
            maxSizeC <- #{peek Poly, maxSize} ptr
            maxDegreeC <- #{peek Poly, maxDeg} ptr
            return (fromCVar maxArityC, fromCSize maxSizeC, fromCPower maxDegreeC)            

{-# INLINE peekArity #-}
peekArity :: (PolyFP cf) -> Var
peekArity p =
    unsafePerformIO $ peekArityIO p
    
{-# INLINE peekArityIO #-}
peekArityIO :: (PolyFP cf) -> IO Var
peekArityIO (PolyFP fp) =
    withForeignPtr fp $ \ptr -> 
        do
        maxArityC <- #{peek Poly, maxArity} ptr
        return (fromCVar maxArityC)

{-# INLINE peekConst #-}
peekConst :: (PolyFP cf) -> cf
peekConst p =
    unsafePerformIO $ peekConstIO p
    
{-# INLINE peekConstIO #-}
peekConstIO :: (PolyFP cf) -> IO cf
peekConstIO (PolyFP fp) =
    withForeignPtr fp $ \ptr -> 
        do
        constSP <- #{peek Poly, constTerm} ptr
        const <- deRefStablePtr constSP
        return const

data Ops_Pure t =
    Ops_Pure
    {
        ops_neg :: {-# UNPACK #-} ! (StablePtr (UnaryOp t)),
        ops_absUp :: {-# UNPACK #-} ! (StablePtr (UnaryOp t)),
        ops_absDn :: {-# UNPACK #-} ! (StablePtr (UnaryOp t)),
        ops_plusUp :: {-# UNPACK #-} ! (StablePtr (BinaryOp t)),
        ops_plusDn :: {-# UNPACK #-} ! (StablePtr (BinaryOp t)),
        ops_minusUp :: {-# UNPACK #-} ! (StablePtr (BinaryOp t)),
        ops_minusDn :: {-# UNPACK #-} ! (StablePtr (BinaryOp t)),
        ops_timesUp :: {-# UNPACK #-} ! (StablePtr (BinaryOp t)),
        ops_timesDn :: {-# UNPACK #-} ! (StablePtr (BinaryOp t))
    }

instance (Storable cf) => Storable (Ops_Pure cf) where
    sizeOf _ = #size Ops_Pure
    alignment _ = #{alignment Ops_Pure}
    peek = error "Ops_Pure.peek: Not needed and not applicable"
    poke ptr (Ops_Pure neg absUp absDn plusUp plusDn minusUp minusDn timesUp timesDn) = 
        do 
        #{poke Ops_Pure, neg} ptr neg
        #{poke Ops_Pure, absUp} ptr absUp
        #{poke Ops_Pure, absDn} ptr absDn
        #{poke Ops_Pure, plusUp} ptr plusUp
        #{poke Ops_Pure, plusDn} ptr plusDn
        #{poke Ops_Pure, minusUp} ptr minusUp
        #{poke Ops_Pure, minusDn} ptr minusDn
        #{poke Ops_Pure, timesUp} ptr timesUp
        #{poke Ops_Pure, timesDn} ptr timesDn

newOps ::
    (Storable cf) =>
    (Ops_Pure cf) ->
    IO (Ptr (Ops_Pure cf))
newOps ops =
    do
    opsP <- malloc
    poke opsP ops
    return opsP
    
freeOps :: (Ptr (Ops_Pure cf)) -> IO ()
freeOps = free

mkOpsPure ::
    (UnaryOp t) ->
    (UnaryOp t) ->
    (UnaryOp t) ->
    (BinaryOp t) ->
    (BinaryOp t) ->
    (BinaryOp t) ->
    (BinaryOp t) ->
    (BinaryOp t) ->
    (BinaryOp t) ->
    IO (Ops_Pure t)
mkOpsPure neg absUp absDn addUp addDn subtrUp subtrDn multUp multDn =
    do
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
      Ops_Pure
        negSP
        absUpSP absDnSP
        addUpSP addDnSP
        subtrUpSP subtrDnSP
        multUpSP multDnSP

mkOpsPureArithUpDn ::
    (ArithUpDn.RoundedReal t) => 
    t ->
    ArithUpDn.RoundedRealEffortIndicator t -> 
    IO (Ops_Pure t)
mkOpsPureArithUpDn sample effort =
    let absEffort = ArithUpDn.rrEffortAbs sample effort in
    let fldEffort = ArithUpDn.rrEffortField sample effort in
    let addEffort = ArithUpDn.fldEffortAdd sample fldEffort in
    let multEffort = ArithUpDn.fldEffortMult sample fldEffort in
    mkOpsPure
        (neg)
        (ArithUpDn.absUpEff absEffort)
        (ArithUpDn.absDnEff absEffort)
        (ArithUpDn.addUpEff addEffort)
        (ArithUpDn.addDnEff addEffort)
        (ArithUpDn.subtrUpEff addEffort)
        (ArithUpDn.subtrDnEff addEffort)
        (ArithUpDn.multUpEff multEffort)
        (ArithUpDn.multDnEff multEffort)

newOpsPureArithUpDn ::
    (ArithUpDn.RoundedReal t, Storable t) => 
    t ->
    ArithUpDn.RoundedRealEffortIndicator t -> 
    Ptr (Ops_Pure t)
newOpsPureArithUpDn sample effort =
    unsafePerformIO $
        do
        ops <- mkOpsPureArithUpDn sample effort
        newOps ops

newOpsPureArithUpDnDefaultEffort ::
    (ArithUpDn.RoundedReal t, Storable t) => 
    t ->
    Ptr (Ops_Pure t)
newOpsPureArithUpDnDefaultEffort sample =
    newOpsPureArithUpDn sample (ArithUpDn.roundedRealDefaultEffort sample)

data Ops_Mutable s t =
    Ops_Mutable
    {
        ops_sample :: {-# UNPACK #-} ! (StablePtr t),
        ops_new :: {-# UNPACK #-} ! (StablePtr (NewOpMutable s t)),
        ops_clone :: {-# UNPACK #-} ! (StablePtr (CloneOpMutable s t)),
        ops_assign :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s t)),
        ops_assignFromPure :: {-# UNPACK #-} ! (StablePtr (UnaryFromPureOpMutable s t)),
--        ops_negMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s t)),
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
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    IO (Ops_Mutable s t)
mkOpsMutable sample new clone assign assignFromPure 
        absUp absDn addUp addDn subtrUp subtrDn multUp multDn =
    do
    sampleSP <- newStablePtr sample
    newSP <- newStablePtr new
    cloneSP <- newStablePtr clone  
    assignSP <- newStablePtr assign  
    assignFromPureSP <- newStablePtr assignFromPure  
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
        (cloneMutable sample)
        (assignMutable sample)
        (writeMutable)
        (ArithUpDn.absUpInPlaceEff sample absEffort)
        (ArithUpDn.absDnInPlaceEff sample absEffort)
        (ArithUpDn.addUpInPlaceEff sample addEffort)
        (ArithUpDn.addDnInPlaceEff sample addEffort)
        (ArithUpDn.subtrUpInPlaceEff sample addEffort)
        (ArithUpDn.subtrDnInPlaceEff sample addEffort)
        (ArithUpDn.multUpInPlaceEff sample multEffort)
        (ArithUpDn.multDnInPlaceEff sample multEffort)

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
        poly_printPoly :: (Ptr (Poly cf)) -> IO ()  

printPoly :: (PolyFP cf) -> IO ()
printPoly (PolyFP fp) =
    withForeignPtr fp $ \ ptr ->
        poly_printPoly ptr

----------------------------------------------------------------

foreign import ccall safe "freePolyGenCf"
        poly_freePoly :: (Ptr (Poly cf)) -> IO ()  

concFinalizerFreePoly :: (Ptr (Poly cf)) -> IO ()
concFinalizerFreePoly p =
    do
--    putStrLn "concFinalizerFreePoly"
    poly_freePoly p

----------------------------------------------------------------
foreign import ccall safe "mapCoeffsInPlaceGenCf"
        poly_mapCoeffsInPlace ::
            (StablePtr (cf1 -> cf2)) -> 
            (Ptr (Poly cf1)) -> 
            IO ()  

unsafeReadPolyMutable ::
    (CanBeMutable cf) =>
    cf ->
    (PolyMutableFP cf s) ->
    ST s (PolyFP cf)
unsafeReadPolyMutable sample (PolyMutableFP fp) =
    do
    unsafeIOToST $ 
        do
        unsafeReadMutableSP <- newStablePtr unsafeReadMutableCoeff
        withForeignPtr fp $ \p ->
            poly_mapCoeffsInPlace unsafeReadMutableSP p 
    return $ PolyFP $ castForeignPtr fp
    where
    unsafeReadMutableCoeff cfM = 
        unsafePerformIO $ unsafeSTToIO $
            do 
            cf <- unsafeReadMutable cfM
            return $ head [cf, sample]
     
----------------------------------------------------------------

foreign import ccall unsafe "newConstPolyGenCf"
        poly_newConstPoly :: 
            (StablePtr cf) -> 
            (StablePtr cf) -> 
            CVar -> CSize -> CPower -> 
            IO (Ptr (Poly cf))  

constPoly :: (Show cf) => cf -> cf -> Var -> Size -> Power -> (PolyFP cf)
constPoly c radius maxArity maxSize maxDeg =
    unsafePerformIO $ newConstPoly c radius maxArity maxSize maxDeg

newConstPoly :: (Show cf) => cf -> cf -> Var -> Size -> Power -> IO (PolyFP cf)
newConstPoly c radius maxArity maxSize maxDeg =
    do
    cSP <- newStablePtr c
    radiusSP <- newStablePtr radius
--    putStrLn $ "calling newConstPoly for " ++ show c
    pP <- poly_newConstPoly cSP radiusSP (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
--    putStrLn $ "newConstPoly for " ++ show c ++ " returned"
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyFP fp

constPolyMutable :: (CanBeMutable cf) => cf -> cf -> Var -> Size -> Power -> ST s (PolyMutableFP cf s)
constPolyMutable c radius maxArity maxSize maxDeg =
    unsafeIOToST $ newConstPolyMutable c radius maxArity maxSize maxDeg


newConstPolyMutable :: (CanBeMutable cf) => cf -> cf -> Var -> Size -> Power -> IO (PolyMutableFP cf s)
newConstPolyMutable c radius maxArity maxSize maxDeg =
    do
    var <- unsafeSTToIO $ makeMutable c
    varSP <- newStablePtr var
    rad <- unsafeSTToIO $ makeMutable radius
    radSP <- newStablePtr rad
    pP <- poly_newConstPoly varSP radSP (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyMutableFP fp


----------------------------------------------------------------

foreign import ccall unsafe "newProjectionPolyGenCf"
        poly_newProjectionPoly :: 
            (StablePtr cf) -> (StablePtr cf) -> (StablePtr cf) ->
            CVar -> CVar -> CSize -> CPower -> 
            IO (Ptr (Poly cf))  

projectionPoly :: 
    (Storable cf, HasOne cf, HasZero cf) => 
    cf -> Var -> Var -> Size -> Power -> (PolyFP cf)
projectionPoly sample x maxArity maxSize maxDeg =
    unsafePerformIO $ newProjectionPoly sample x maxArity maxSize maxDeg

newProjectionPoly :: 
    (Storable cf, HasOne cf, HasZero cf) => 
    cf -> Var -> Var -> Size -> Power -> IO (PolyFP cf)
newProjectionPoly _sample x maxArity maxSize maxDeg =
    do
    zeroSP <- newStablePtr zero
    oneSP <- newStablePtr one
    radSP <- newStablePtr zero
    pP <- poly_newProjectionPoly zeroSP oneSP radSP (toCVar x) (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyFP fp

projectionPolyMutable :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    cf -> Var -> Var -> Size -> Power -> ST s (PolyMutableFP cf s)
projectionPolyMutable sample x maxArity maxSize maxDeg =
    unsafeIOToST $ newProjectionPolyMutable sample x maxArity maxSize maxDeg


newProjectionPolyMutable :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    cf -> Var -> Var -> Size -> Power -> IO (PolyMutableFP cf s)
newProjectionPolyMutable sample x maxArity maxSize maxDeg =
    do
    zeroM <- unsafeSTToIO $ makeMutable $ head [zero, sample]
    zeroSP <- newStablePtr zeroM
    oneM <- unsafeSTToIO $ makeMutable $ head [one, sample]
    oneSP <- newStablePtr oneM
    radM <- unsafeSTToIO $ makeMutable $ head [zero, sample]
    radSP <- newStablePtr radM
    pP <- poly_newProjectionPoly zeroSP oneSP radSP (toCVar x) (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyMutableFP fp

----------------------------------------------------------------

foreign import ccall safe "addUpUsingPureOpsGenCf"
        poly_addUpUsingPureOps :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp cf)) ->
            (Ptr (Ops_Pure cf)) ->
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            IO ()

foreign import ccall safe "addDnUsingPureOpsGenCf"
        poly_addDnUsingPureOps :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp cf)) ->
            (Ptr (Ops_Pure cf)) ->
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            IO ()

polyAddUpPureUsingPureOps ::
    (HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    Size ->
    Power ->
    (Ptr (Ops_Pure cf)) ->
    PolyFP cf ->
    PolyFP cf ->
    PolyFP cf
polyAddUpPureUsingPureOps zero size deg opsPtr = 
    polyBinaryOpPure poly_addUpUsingPureOps zero size deg opsPtr

polyAddDnPureUsingPureOps ::
    (HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    Size ->
    Power ->
    (Ptr (Ops_Pure cf)) ->
    PolyFP cf ->
    PolyFP cf ->
    PolyFP cf
polyAddDnPureUsingPureOps zero size deg opsPtr = 
    polyBinaryOpPure poly_addDnUsingPureOps zero size deg opsPtr

polyBinaryOpPure binaryOp sample maxSize maxDeg opsPtr p1@(PolyFP p1FP) (PolyFP p2FP) =
    unsafePerformIO $
    do
    maxArity <- peekArityIO p1
    zeroSP <- newStablePtr $ head [zero, sample]
    radSP <- newStablePtr $ head [zero, sample]
    resP <- poly_newConstPoly zeroSP radSP (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
    compareSP <- newStablePtr $ (NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort sample))
    _ <- withForeignPtr p1FP $ \p1P ->
             withForeignPtr p2FP $ \p2P ->
                 binaryOp zeroSP compareSP opsPtr resP p1P p2P
    freeStablePtr compareSP
    resFP <- Conc.newForeignPtr resP (concFinalizerFreePoly resP)
    return $ PolyFP resFP

----------------------------------------------------------------

foreign import ccall safe "addUpUsingMutableOpsGenCf"
        poly_addUpUsingMutableOps :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

polyAddUpMutableUsingMutableOps ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    (Ptr (Ops_Mutable s cf)) ->
    PolyMutableFP cf s ->
    PolyMutableFP cf s ->
    PolyMutableFP cf s ->
    ST s ()
polyAddUpMutableUsingMutableOps sample opsMutablePtr = 
    polyBinaryOpMutable poly_addUpUsingMutableOps sample opsMutablePtr

polyBinaryOpMutable binaryOp sample opsMutablePtr 
        (PolyMutableFP resFP) (PolyMutableFP p1FP) (PolyMutableFP p2FP) =
    unsafeIOToST $
    do
    zeroSP <- newStablePtr $ head [zero, sample]
    compareSP <- newStablePtr compareMutable
    _ <- withForeignPtr p1FP $ \p1P ->
             withForeignPtr p2FP $ \p2P ->
                 withForeignPtr resFP $ \resP ->
                     binaryOp zeroSP compareSP opsMutablePtr resP p1P p2P
    freeStablePtr compareSP
    where
    compareMutable v1M v2M =
        unsafePerformIO $ unsafeSTToIO $
        do
        v1 <- unsafeReadMutable v1M
        v2 <- unsafeReadMutable v2M
        let _ = [v1,v2,sample] 
        return $ NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort sample) v1 v2

----------------------------------------------------------------

foreign import ccall safe "evalAtPtChebBasisGenCf"
        poly_evalAtPtChebBasis :: 
            (Ptr (Poly cf)) -> 
            (Ptr (StablePtr val)) -> 
            (StablePtr val) ->
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) ->
            (StablePtr (ConvertOp cf val)) ->
            IO (StablePtr val)  

evalAtPtChebBasis :: 
    (Storable cf) => 
    (PolyFP cf) ->
    [val] {-^ values to substitute for variables @[0..(maxArity-1)]@ -} ->
    val {-^ number @1@ -} ->
    (BinaryOp val) {-^ addition -} -> 
    (BinaryOp val) {-^ subtraction -} -> 
    (BinaryOp val) {-^ multiplication -} -> 
    (ConvertOp cf val) ->
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

foreign import ccall safe "evalAtPtPowerBasisGenCf"
        poly_evalAtPtPowerBasis :: 
            (Ptr (Poly cf)) -> 
            (Ptr (StablePtr val)) -> 
            (StablePtr val) ->
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) ->
            (StablePtr (ConvertOp cf val)) ->
            IO (StablePtr val)  

evalAtPtPowerBasis :: 
    (Storable cf) => 
    (PolyFP cf) ->
    [val] {-^ values to substitute for variables @[0..(maxArity-1)]@ -} ->
    val {-^ number @1@ -} ->
    (BinaryOp val) {-^ addition -} -> 
    (BinaryOp val) {-^ multiplication -} -> 
    (ConvertOp cf val) ->
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
    
----------------------------------------------------------------

foreign import ccall safe "boundUpThinGenCf"
    poly_boundUpThin ::
        (Ptr (Ops_Pure cf)) ->
        (Ptr (Poly cf)) -> 
        IO (StablePtr cf)

polyBoundUpThin :: 
    (HasZero cf, NumOrd.PartialComparison cf) =>
    (Ptr (Ops_Pure cf)) ->
    PolyFP cf ->
    cf
polyBoundUpThin opsPtr =
    polyEval poly_boundUpThin opsPtr

foreign import ccall safe "boundDnThinGenCf"
    poly_boundDnThin ::
        (Ptr (Ops_Pure cf)) ->
        (Ptr (Poly cf)) -> 
        IO (StablePtr cf)

polyBoundDnThin :: 
    (HasZero cf, NumOrd.PartialComparison cf) =>
    (Ptr (Ops_Pure cf)) ->
    PolyFP cf ->
    cf
polyBoundDnThin opsPtr =
    polyEval poly_boundDnThin opsPtr

foreign import ccall safe "boundUpGenCf"
    poly_boundUp ::
        (Ptr (Ops_Pure cf)) ->
        (Ptr (Poly cf)) -> 
        IO (StablePtr cf)

polyBoundUp :: 
    (HasZero cf, NumOrd.PartialComparison cf) =>
    (Ptr (Ops_Pure cf)) ->
    PolyFP cf ->
    cf
polyBoundUp opsPtr =
    polyEval poly_boundUp opsPtr

foreign import ccall safe "boundDnGenCf"
    poly_boundDn ::
        (Ptr (Ops_Pure cf)) ->
        (Ptr (Poly cf)) -> 
        IO (StablePtr cf)

polyBoundDn :: 
    (HasZero cf, NumOrd.PartialComparison cf) =>
    (Ptr (Ops_Pure cf)) ->
    PolyFP cf ->
    cf
polyBoundDn opsPtr =
    polyEval poly_boundDn opsPtr

polyEval unary ops (PolyFP pFP) =
    unsafePerformIO $
    do
    resSP <- withForeignPtr pFP $ \p ->
        unary ops p
    res <- deRefStablePtr resSP
    return res
