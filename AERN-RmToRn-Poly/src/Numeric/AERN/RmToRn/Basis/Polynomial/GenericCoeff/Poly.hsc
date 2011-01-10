{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, EmptyDataDecls #-}
#include <GenericCoeff/poly.h>

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Poly where

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

import Data.Typeable(Typeable)

--import Data.Function(on)
    
type CVar = #type Var
type CSize = #type Size
type CPower = #type Power

newtype Var = Var Word32 deriving (Eq, Ord, Show, Enum)
{-# INLINE fromCVar #-}
fromCVar :: CVar -> Var
fromCVar v = Var (fromIntegral v)
{-# INLINE toCVar #-}
toCVar :: Var -> CVar
toCVar (Var v) = fromIntegral v

newtype Size = Size Word32 deriving (Eq, Ord, Show, Enum)
{-# INLINE fromCSize #-}
fromCSize :: CSize -> Size
fromCSize s = Size (fromIntegral s)
{-# INLINE toCSize #-}
toCSize :: Size -> CSize
toCSize (Size s) = fromIntegral s

newtype Power = Power Word32 deriving (Eq, Ord, Show, Enum)
{-# INLINE fromCPower #-}
fromCPower :: CPower -> Power
fromCPower pwr = Power (fromIntegral pwr)
{-# INLINE toCPower #-}
toCPower :: Power -> CPower
toCPower (Power pwr) = fromIntegral pwr
    
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Poly cf -- defined in poly.c, opaque to Haskell  

newtype PolyFP cf = PolyFP (ForeignPtr (Poly cf))
newtype PolyMutableFP cf s = PolyMutableFP (ForeignPtr (Poly (Mutable cf s)))

{-# INLINE peekSizes #-}
peekSizes :: (PolyFP cf) -> (Var, Size)
peekSizes p =
    unsafePerformIO $ peekSizesIO p

peekSizesIO :: (PolyFP cf) -> IO (Var, Size)
peekSizesIO (PolyFP fp) =
        withForeignPtr fp $ \ptr -> 
            do
            maxArityC <- #{peek Poly, maxArity} ptr
            maxSizeC <- #{peek Poly, maxSize} ptr
            return (fromCVar maxArityC, fromCSize maxSizeC)            

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

data Ops_Pure t =
    Ops_Pure
    {
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
    poke ptr (Ops_Pure absUp absDn plusUp plusDn minusUp minusDn timesUp timesDn) = 
        do 
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
    (BinaryOp t) ->
    (BinaryOp t) ->
    (BinaryOp t) ->
    (BinaryOp t) ->
    (BinaryOp t) ->
    (BinaryOp t) ->
    IO (Ops_Pure t)
mkOpsPure absUp absDn addUp addDn subtrUp subtrDn multUp multDn =
    do
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
              sample new clone assign 
              absUpMutable absDnMutable 
              plusUpMutable plusDnMutable 
              minusUpMutable minusDnMutable 
              timesUpMutable timesDnMutable) = 
        do 
        #{poke Ops_Mutable, sample} ptr sample
        #{poke Ops_Mutable, new} ptr new
        #{poke Ops_Mutable, clone} ptr clone
        #{poke Ops_Mutable, assign} ptr assign
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
    (UnaryOpMutable s t) ->
    (UnaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    (BinaryOpMutable s t) ->
    IO (Ops_Mutable s t)
mkOpsMutable sample new clone assign absUp absDn addUp addDn subtrUp subtrDn multUp multDn =
    do
    sampleSP <- newStablePtr sample
    newSP <- newStablePtr new
    cloneSP <- newStablePtr clone  
    assignSP <- newStablePtr assign  
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
        cloneSP assignSP
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

foreign import ccall unsafe "freePoly"
        poly_freePoly :: (Ptr (Poly cf)) -> IO ()  

concFinalizerFreePoly :: (Ptr (Poly cf)) -> IO ()
concFinalizerFreePoly = poly_freePoly

----------------------------------------------------------------
foreign import ccall safe "mapCoeffsInPlace"
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

foreign import ccall unsafe "newConstPoly"
        poly_newConstPoly :: 
            (StablePtr cf) -> 
            CVar -> CSize -> 
            IO (Ptr (Poly cf))  

constPoly :: cf -> Var -> Size -> (PolyFP cf)
constPoly c maxArity maxSize =
    unsafePerformIO $ newConstPoly c maxArity maxSize

newConstPoly :: cf -> Var -> Size -> IO (PolyFP cf)
newConstPoly c maxArity maxSize =
    do
    cSP <- newStablePtr c
    pP <- poly_newConstPoly cSP (toCVar maxArity) (toCSize maxSize)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyFP fp

constPolyMutable :: (CanBeMutable cf) => cf -> Var -> Size -> ST s (PolyMutableFP cf s)
constPolyMutable c maxArity maxSize =
    unsafeIOToST $ newConstPolyMutable c maxArity maxSize


newConstPolyMutable :: (CanBeMutable cf) => cf -> Var -> Size -> IO (PolyMutableFP cf s)
newConstPolyMutable c maxArity maxSize =
    do
    var <- unsafeSTToIO $ makeMutable c
    varSP <- newStablePtr var
    pP <- poly_newConstPoly varSP (toCVar maxArity) (toCSize maxSize)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyMutableFP fp


----------------------------------------------------------------

foreign import ccall unsafe "newProjectionPoly"
        poly_newProjectionPoly :: 
            (StablePtr cf) -> (StablePtr cf) -> 
            CVar -> CVar -> CSize -> 
            IO (Ptr (Poly cf))  

projectionPoly :: 
    (Storable cf, HasOne cf, HasZero cf) => 
    cf -> Var -> Var -> Size -> (PolyFP cf)
projectionPoly sample x maxArity maxSize =
    unsafePerformIO $ newProjectionPoly sample x maxArity maxSize

newProjectionPoly :: 
    (Storable cf, HasOne cf, HasZero cf) => 
    cf -> Var -> Var -> Size -> IO (PolyFP cf)
newProjectionPoly _sample x maxArity maxSize =
    do
    zeroSP <- newStablePtr zero
    oneSP <- newStablePtr one
    pP <- poly_newProjectionPoly zeroSP oneSP (toCVar x) (toCVar maxArity) (toCSize maxSize)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyFP fp

projectionPolyMutable :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    cf -> Var -> Var -> Size -> ST s (PolyMutableFP cf s)
projectionPolyMutable sample x maxArity maxSize =
    unsafeIOToST $ newProjectionPolyMutable sample x maxArity maxSize


newProjectionPolyMutable :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    cf -> Var -> Var -> Size -> IO (PolyMutableFP cf s)
newProjectionPolyMutable sample x maxArity maxSize =
    do
    zeroM <- unsafeSTToIO $ makeMutable $ head [zero, sample]
    zeroSP <- newStablePtr zeroM
    oneM <- unsafeSTToIO $ makeMutable $ head [one, sample]
    oneSP <- newStablePtr oneM
    pP <- poly_newProjectionPoly zeroSP oneSP (toCVar x) (toCVar maxArity) (toCSize maxSize)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyMutableFP fp

----------------------------------------------------------------

foreign import ccall safe "addUpUsingPureOps"
        poly_addUpUsingPureOps :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp cf)) ->
            (Ptr (Ops_Pure cf)) ->
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            IO ()

foreign import ccall safe "addDnUsingPureOps"
        poly_addDnUsingPureOps :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp cf)) ->
            (Ptr (Ops_Pure cf)) ->
            (Ptr (Ops_Mutable s cf)) ->
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            IO ()

polyAddUpPureUsingPureOps ::
    (HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    Size ->
    (Ptr (Ops_Pure cf)) ->
    PolyFP cf ->
    PolyFP cf ->
    PolyFP cf
polyAddUpPureUsingPureOps zero size opsPtr = 
    polyBinaryOpPure poly_addUpUsingPureOps zero size opsPtr nullPtr

polyAddDnPureUsingPureOps ::
    (HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    Size ->
    (Ptr (Ops_Pure cf)) ->
    PolyFP cf ->
    PolyFP cf ->
    PolyFP cf
polyAddDnPureUsingPureOps zero size opsPtr = 
    polyBinaryOpPure poly_addDnUsingPureOps zero size opsPtr nullPtr

polyBinaryOpPure binaryOp sample maxSize opsPtr opsMutablePtr p1@(PolyFP p1FP) (PolyFP p2FP) =
    unsafePerformIO $
    do
    maxArity <- peekArityIO p1
    zeroSP <- newStablePtr $ head [zero, sample]
    resP <- poly_newConstPoly zeroSP (toCVar maxArity) (toCSize maxSize)
    compareSP <- newStablePtr $ (NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort sample))
    _ <- withForeignPtr p1FP $ \p1 ->
             withForeignPtr p2FP $ \p2 ->
                 binaryOp zeroSP compareSP opsPtr opsMutablePtr resP p1 p2
    freeStablePtr compareSP
    resFP <- Conc.newForeignPtr resP (concFinalizerFreePoly resP)
    return $ PolyFP resFP

----------------------------------------------------------------

foreign import ccall safe "addUpUsingMutableOps"
        poly_addUpUsingMutableOps :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (Ptr (Ops_Pure cf)) ->
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
    polyBinaryOpMutable poly_addUpUsingMutableOps sample nullPtr opsMutablePtr

polyBinaryOpMutable binaryOp sample opsPtr opsMutablePtr 
        (PolyMutableFP resFP) (PolyMutableFP p1FP) (PolyMutableFP p2FP) =
    unsafeIOToST $
    do
    zeroSP <- newStablePtr $ head [zero, sample]
    compareSP <- newStablePtr compareMutable
    _ <- withForeignPtr p1FP $ \p1 ->
             withForeignPtr p2FP $ \p2 ->
                 withForeignPtr resFP $ \resP ->
                     binaryOp zeroSP compareSP opsPtr opsMutablePtr resP p1 p2
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

foreign import ccall safe "testAssign"
        poly_testAssign :: 
            (StablePtr t) ->
            (StablePtr (UnaryOpMutable s t)) ->
            (StablePtr (Mutable t s)) ->
            (StablePtr (Mutable t s)) ->
            IO ()
            
testAssign ::
    (CanBeMutable t) =>
    t -> 
    Mutable t s -> 
    Mutable t s -> 
    ST s ()
testAssign sample to from =
    do
    fromV <- unsafeReadMutable from
    toV <- unsafeReadMutable to
    let _ = [fromV, toV, sample]
    unsafeIOToST $ 
        do
        sampleSP <- newStablePtr sample
        assignSP <- newStablePtr $ \ to from -> assignMutable sample to from
        toSP <- newStablePtr to
        fromSP <- newStablePtr from
        poly_testAssign sampleSP assignSP toSP fromSP
    
      
----------------------------------------------------------------

foreign import ccall safe "evalAtPtChebBasis"
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
    res <- deRefStablePtr resSP 
    freeStablePtr resSP
    return res
    