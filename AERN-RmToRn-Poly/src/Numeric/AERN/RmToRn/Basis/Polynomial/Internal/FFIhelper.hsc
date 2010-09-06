{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
#include <poly.h>

module Numeric.AERN.RmToRn.Basis.Polynomial.Internal.FFIhelper where

import Numeric.AERN.Basics.PartialOrdering
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Data.Word

import Numeric.AERN.Basics.Mutable
import Control.Monad.ST (ST)

--import Foreign.C.String(CString)
--import Foreign.C.Types(CULong, CLong, CInt, CUInt, CDouble, CChar)
import Foreign.Ptr(Ptr)
--import Foreign.Marshal(alloca)
import Foreign.Storable
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Marshal.Array (newArray)
import Foreign.StablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr) --, mallocForeignPtrBytes)
import qualified Foreign.Concurrent as Conc (newForeignPtr)

import Data.Typeable(Typeable)

--import Data.Function(on)
    
type CVar = #type Var
type CSize = #type Size
type CPower = #type Power

newtype Var = Var { fromVar :: Word32 } deriving (Eq, Ord, Show, Enum)
{-# INLINE fromCVar #-}
fromCVar :: CVar -> Var
fromCVar v = Var (fromIntegral v)
{-# INLINE toCVar #-}
toCVar :: Var -> CVar
toCVar (Var v) = fromIntegral v

newtype Size = Size { fromSize :: Word32 } deriving (Eq, Ord, Show, Enum)
{-# INLINE fromCSize #-}
fromCSize :: CSize -> Size
fromCSize s = Size (fromIntegral s)
{-# INLINE toCSize #-}
toCSize :: Size -> CSize
toCSize (Size s) = fromIntegral s

newtype Power = Power { fromPower :: Word32 } deriving (Eq, Ord, Show, Enum)
{-# INLINE fromCPower #-}
fromCPower :: CPower -> Power
fromCPower pwr = Power (fromIntegral pwr)
{-# INLINE toCPower #-}
toCPower :: Power -> CPower
toCPower (Power pwr) = fromIntegral pwr
    
data Term cf = 
    Term
    { 
        term_powers :: {-# UNPACK #-} !(ForeignPtr CPower),
        term_coeff :: {-# UNPACK #-} !(StablePtr cf)
    } 
    deriving (Typeable)

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance (Storable cf) => Storable (Term cf) where
    sizeOf _ = #size Term
    alignment _ = #{alignment Var}
    peek =
--        do
--        powers <- #{peek Term, powers} ptr
--        coeff <- #{peek Term, coeff} ptr
--        return $ Term powers coeff
        error "Term.peek: Not needed and not applicable"
    poke ptr (Term powers cf) = 
        do 
        withForeignPtr powers $ \p1 -> #{poke Term, powers} ptr p1
        #{poke Term, coeff} ptr cf

newtype PolyFP cf = PolyFP (ForeignPtr (Poly cf))

data Poly cf = 
    Poly 
    { 
        poly_maxArity :: {-# UNPACK #-} ! CVar,
        poly_maxSize :: {-# UNPACK #-} ! CSize,
        poly_psize :: {-# UNPACK #-} ! CSize,
        poly_constTerm :: {-# UNPACK #-} !(StablePtr cf),
        poly_terms :: {-# UNPACK #-} !(ForeignPtr (Term cf))
    } 
    deriving (Typeable)

instance (Storable cf) => Storable (Poly cf) where
    sizeOf _ = #size Poly
    alignment _ = #{alignment Poly}
    peek = error "Poly.peek: Not needed and not applicable"
    poke ptr (Poly maxArity maxSize psize constTerm terms) = 
        do 
        #{poke Poly, maxArity} ptr maxArity
        #{poke Poly, maxSize} ptr maxSize
        #{poke Poly, psize} ptr psize
        #{poke Poly, constTerm} ptr constTerm
        withForeignPtr terms $ \p1 -> #{poke Poly, terms} ptr p1


{-# INLINE peekSizes #-}
peekSizes      :: (PolyFP cf) -> IO (Var, Size)
peekSizes (PolyFP fp) =
    withForeignPtr fp $ \ptr -> 
            do
            maxArityC <- #{peek Poly, maxArity} ptr
            maxSizeC <- #{peek Poly, maxSize} ptr
            return (fromCVar maxArityC, fromCSize maxSizeC)            

{-# INLINE peekArity #-}
peekArity      :: (PolyFP cf) -> IO (Var)
peekArity (PolyFP fp) =
    withForeignPtr fp $ \ptr -> 
            do
            maxArityC <- #{peek Poly, maxArity} ptr
            return (fromCVar maxArityC)            

type UnaryOp t = t -> t
type BinaryOp t =  t -> t -> t
type ConvertOp t1 t2 = t1 -> t2
type ComparisonOp t =  t -> t -> (Maybe PartialOrdering)

type UnaryOpInPlace s t = Mutable s t -> Mutable s t -> ST s ()
type BinaryOpInPlace s t = Mutable s t -> Mutable s t -> Mutable s t -> ST s ()

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

data Ops_InPlace s t =
    Ops_InPlace
    {
        ops_absUpInPlace :: {-# UNPACK #-} ! (StablePtr (UnaryOpInPlace s t)),
        ops_absDnInPlace :: {-# UNPACK #-} ! (StablePtr (UnaryOpInPlace s t)),
        ops_plusUpInPlace :: {-# UNPACK #-} ! (StablePtr (BinaryOpInPlace s t)),
        ops_plusDnInPlace :: {-# UNPACK #-} ! (StablePtr (BinaryOpInPlace s t)),
        ops_minusUpInPlace :: {-# UNPACK #-} ! (StablePtr (BinaryOpInPlace s t)),
        ops_minusDnInPlace :: {-# UNPACK #-} ! (StablePtr (BinaryOpInPlace s t)),
        ops_timesUpInPlace :: {-# UNPACK #-} ! (StablePtr (BinaryOpInPlace s t)),
        ops_timesDnInPlace :: {-# UNPACK #-} ! (StablePtr (BinaryOpInPlace s t))
    }

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

instance (Storable cf) => Storable (Ops_InPlace s cf) where
    sizeOf _ = #size Ops_InPlace
    alignment _ = #{alignment Ops_InPlace}
    peek = error "Ops_InPlace.peek: Not needed and not applicable"
    poke ptr 
            (Ops_InPlace absUpInPlace absDnInPlace 
             plusUpInPlace plusDnInPlace 
             minusUpInPlace minusDnInPlace 
             timesUpInPlace timesDnInPlace) = 
        do 
        #{poke Ops_InPlace, absUpInPlace} ptr absUpInPlace
        #{poke Ops_InPlace, absDnInPlace} ptr absDnInPlace
        #{poke Ops_InPlace, plusUpInPlace} ptr plusUpInPlace
        #{poke Ops_InPlace, plusDnInPlace} ptr plusDnInPlace
        #{poke Ops_InPlace, minusUpInPlace} ptr minusUpInPlace
        #{poke Ops_InPlace, minusDnInPlace} ptr minusDnInPlace
        #{poke Ops_InPlace, timesUpInPlace} ptr timesUpInPlace
        #{poke Ops_InPlace, timesDnInPlace} ptr timesDnInPlace

----------------------------------------------------------------

foreign import ccall unsafe "freePoly"
        poly_freePoly :: (Ptr (Poly cf)) -> IO ()  

concFinalizerFreePoly :: (Ptr (Poly cf)) -> IO ()
concFinalizerFreePoly = poly_freePoly

----------------------------------------------------------------

foreign import ccall unsafe "newConstPoly"
        poly_newConstPoly :: 
            (StablePtr cf) -> 
            CVar -> CSize -> 
            IO (Ptr (Poly cf))  

newConstPoly :: cf -> Var -> Size -> IO (PolyFP cf)
newConstPoly c maxArity maxSize =
    do
    cSP <- newStablePtr c
    pP <- poly_newConstPoly cSP (toCVar maxArity) (toCSize maxSize)
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ PolyFP fp

----------------------------------------------------------------

foreign import ccall unsafe "newProjectionPoly"
        poly_newProjectionPoly :: 
            (StablePtr cf) -> (StablePtr cf) -> 
            CVar -> CVar -> CSize -> 
            IO (Ptr (Poly cf))  

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

----------------------------------------------------------------

foreign import ccall safe "addUp"
        poly_addUp :: 
            (StablePtr cf) ->
            (StablePtr (ComparisonOp cf)) ->
            (Ptr (Ops_Pure cf)) ->
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            (Ptr (Poly cf)) -> 
            IO ()

polyAddUp :: 
    (HasZero cf, NumOrd.PartialComparison cf) =>
    cf -> 
    Size ->
    (Ptr (Ops_Pure cf)) ->
    PolyFP cf ->
    PolyFP cf ->
    IO (PolyFP cf)
polyAddUp sample maxSize opsPtr p1@(PolyFP p1FP) (PolyFP p2FP) =
    do
    maxArity <- peekArity p1
    zeroSP <- newStablePtr $ head [zero, sample]
    resP <- poly_newConstPoly zeroSP (toCVar maxArity) (toCSize maxSize)
    compareSP <- newStablePtr $ (NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort sample))
    withForeignPtr p1FP $ \p1 ->
        withForeignPtr p2FP $ \p2 ->
            poly_addUp zeroSP compareSP opsPtr resP p1 p2
    freeStablePtr compareSP
    resFP <- Conc.newForeignPtr resP (concFinalizerFreePoly resP)
    return $ PolyFP resFP

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
    val -> {-^ number @1@ -}
    (BinaryOp val) {-^ addition -} -> 
    (BinaryOp val) {-^ subtraction -} -> 
    (BinaryOp val) {-^ multiplication -} -> 
    (ConvertOp cf val) ->
    IO val
evalAtPtChebBasis (PolyFP polyFP) vals one add subtr mult cf2val =
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
    