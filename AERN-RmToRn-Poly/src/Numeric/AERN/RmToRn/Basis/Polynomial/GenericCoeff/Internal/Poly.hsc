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

--import Data.Typeable(Typeable)
--import Data.Function(on)
    
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

printPoly :: (PolyFP cf s) -> IO ()
printPoly (PolyFP fp) =
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

--unsafeReadPoly ::
--    (CanBeMutable cf) =>
--    cf ->
--    (PolyFP cf s) ->
--    ST s (PolyFP cf)
--unsafeReadPoly sample (PolyFP fp) =
--    do
--    unsafeIOToST $ 
--        do
--        unsafeReadMutableSP <- newStablePtr unsafeReadMutableCoeff
--        withForeignPtr fp $ \p ->
--            poly_mapCoeffsInPlace unsafeReadMutableSP p 
--    return $ PolyFP $ castForeignPtr fp
--    where
--    unsafeReadMutableCoeff cfM = 
--        unsafePerformIO $ unsafeSTToIO $
--            do 
--            cf <- unsafeReadMutable cfM
--            return $ head [cf, sample]
--     
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
    unsafeIOToST $ newConstPoly cM radiusM maxArity maxSize maxDeg

newConstPoly :: 
    (Show cf, CanBeMutable cf) => 
    cf -> cf -> 
    Var -> Size -> Power -> 
    IO (PolyFP cf s)
newConstPoly c radius maxArity maxSize maxDeg =
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
    cf -> Var -> Var -> Size -> Power -> ST s (PolyFP cf s)
projectionPoly sample x maxArity maxSize maxDeg =
    unsafeIOToST $ newProjectionPoly sample x maxArity maxSize maxDeg


newProjectionPoly :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    cf -> Var -> Var -> Size -> Power -> IO (PolyFP cf s)
newProjectionPoly sample x maxArity maxSize maxDeg =
    do
    zeroM <- unsafeSTToIO $ makeMutable $ head [zero, sample]
    zeroSP <- newStablePtr zeroM
    oneM <- unsafeSTToIO $ makeMutable $ head [one, sample]
    oneSP <- newStablePtr oneM
    radM <- unsafeSTToIO $ makeMutable $ head [zero, sample]
    radSP <- newStablePtr radM
    pP <- poly_newProjectionPoly zeroSP oneSP radSP (toCVar x) (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyFP fp


----------------------------------------------------------------

foreign import ccall safe "addUpGenCf"
        poly_addUp :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

polyAddUpMutable ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    (Ptr (Ops_Mutable s cf)) ->
    PolyFP cf s ->
    PolyFP cf s ->
    PolyFP cf s ->
    ST s ()
polyAddUpMutable sample opsMutablePtr = 
    polyBinaryOpMutable poly_addUp sample opsMutablePtr

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

polyScaleUpMutable ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    (Ptr (Ops_Mutable s cf)) ->
    cf ->
    PolyFP cf s ->
    ST s ()
polyScaleUpMutable sample opsMutablePtr = 
    polyScalingOpMutable poly_scaleUpThin sample opsMutablePtr

polyScaleDnMutable ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    (Ptr (Ops_Mutable s cf)) ->
    cf ->
    PolyFP cf s ->
    ST s ()
polyScaleDnMutable sample opsMutablePtr = 
    polyScalingOpMutable poly_scaleDnThin sample opsMutablePtr

polyScaleEnclMutable ::
    (CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    (Ptr (Ops_Mutable s cf)) ->
    cf ->
    PolyFP cf s ->
    ST s ()
polyScaleEnclMutable opsMutablePtr = 
    polyScalingOpMutableNoZero poly_scaleEncl opsMutablePtr

----------------------------------------------------------------

polyScalingOpMutable scalingOp sample opsMutablePtr scalingFactor (PolyFP pFP) =
    do
    sfM <- unsafeMakeMutable scalingFactor 
    unsafeIOToST $
      do
      zeroSP <- newStablePtr $ head [zero, sample]
      factorSP <- newStablePtr sfM
      _ <- withForeignPtr pFP $ \pP ->
            scalingOp zeroSP opsMutablePtr factorSP pP
      return ()

----------------------------------------------------------------

polyScalingOpMutableNoZero scalingOp opsMutablePtr scalingFactor (PolyFP pFP) =
    do
    sfM <- unsafeMakeMutable scalingFactor 
    unsafeIOToST $
      do
      factorSP <- newStablePtr sfM
      _ <- withForeignPtr pFP $ \pP ->
            scalingOp opsMutablePtr factorSP pP
      return ()

--------------------------------------------------------------

polyBinaryOpMutable binaryOp sample opsMutablePtr 
        (PolyFP resFP) (PolyFP p1FP) (PolyFP p2FP) =
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
            (Ptr (Poly cfm)) -> 
            (Ptr (StablePtr val)) -> 
            (StablePtr val) ->
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) ->
            (StablePtr (ConvertOp cfm val)) ->
            IO (StablePtr val)

evalAtPtChebBasis :: 
    (Storable cf, CanBeMutable cf) => 
    (PolyFP cf s) ->
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
            (Ptr (Poly cfm)) -> 
            (Ptr (StablePtr val)) -> 
            (StablePtr val) ->
            (StablePtr (BinaryOp val)) -> 
            (StablePtr (BinaryOp val)) ->
            (StablePtr (ConvertOp cfm val)) ->
            IO (StablePtr val)  

evalAtPtPowerBasis :: 
    (Storable cf, CanBeMutable cf) => 
    (PolyFP cf s) ->
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
        (Ptr (Ops_Mutable s cf)) ->
        (StablePtr cfm) -> 
        (Ptr (Poly cfm)) -> 
        IO ()

polyBoundUpThin :: 
    (HasZero cf, NumOrd.PartialComparison cf, CanBeMutable cf) =>
    (Ptr (Ops_Mutable s cf)) ->
    Mutable cf s ->
    PolyFP cf s ->
    ST s ()
polyBoundUpThin opsPtr =
    polyEval poly_boundUpThin opsPtr

foreign import ccall safe "boundDnThinGenCf"
    poly_boundDnThin ::
        (Ptr (Ops_Mutable s cf)) ->
        (StablePtr cfm) -> 
        (Ptr (Poly cfm)) -> 
        IO ()

polyBoundDnThin :: 
    (HasZero cf, NumOrd.PartialComparison cf, CanBeMutable cf) =>
    (Ptr (Ops_Mutable s cf)) ->
    Mutable cf s ->
    PolyFP cf s ->
    ST s ()
polyBoundDnThin opsPtr =
    polyEval poly_boundDnThin opsPtr

foreign import ccall safe "boundUpGenCf"
    poly_boundUp ::
        (Ptr (Ops_Mutable s cf)) ->
        (StablePtr cfm) -> 
        (Ptr (Poly cfm)) -> 
        IO ()

polyBoundUp :: 
    (HasZero cf, NumOrd.PartialComparison cf, CanBeMutable cf) =>
    (Ptr (Ops_Mutable s cf)) ->
    Mutable cf s ->
    PolyFP cf s ->
    ST s ()
polyBoundUp opsPtr =
    polyEval poly_boundUp opsPtr

foreign import ccall safe "boundDnGenCf"
    poly_boundDn ::
        (Ptr (Ops_Mutable s cf)) ->
        (StablePtr cfm) -> 
        (Ptr (Poly cfm)) -> 
        IO ()

polyBoundDn :: 
    (HasZero cf, NumOrd.PartialComparison cf, CanBeMutable cf) =>
    (Ptr (Ops_Mutable s cf)) ->
    Mutable cf s ->
    PolyFP cf s ->
    ST s ()
polyBoundDn opsPtr =
    polyEval poly_boundDn opsPtr

polyEval unary ops resM (PolyFP pFP) =
    unsafeIOToST $
    do
    resMST <- newStablePtr resM
    withForeignPtr pFP $ \p ->
        unary ops resMST p
    freeStablePtr resMST

----------------------------------------------------------------
{-

foreign import ccall safe "reduceDegreeEnclGenCf"
        poly_reduceDegreeEncl :: 
            (Ptr (Ops_Mutable s cf)) ->
            CPower ->
            (Ptr (Poly (Mutable cf s))) -> 
            IO ()

polyReduceDegreeEnclMutable ::
    (CanBeMutable cf) =>
    (Ptr (Ops_Mutable s cf)) ->
    Power ->
    PolyFP cf s ->
    ST s ()
polyReduceDegreeEnclMutable opsMutablePtr = 
    polyReductionOpMutable poly_reduceDegreeEncl opsMutablePtr

polyReductionOpMutable reductionOp opsMutablePtr maxDeg (PolyFP pFP) =
    do
    unsafeIOToST $
      do
      _ <- withForeignPtr pFP $ \pP ->
            reductionOp opsMutablePtr (toCPower maxDeg) pP
      return ()

-}
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
        
