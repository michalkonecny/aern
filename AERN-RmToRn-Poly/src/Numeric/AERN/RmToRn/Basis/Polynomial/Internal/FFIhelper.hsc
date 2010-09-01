{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
#include <poly.h>

module Numeric.AERN.RmToRn.Basis.Polynomial.Internal.FFIhelper where

import Numeric.AERN.Basics.Laws.Utilities

import Numeric.AERN.RealArithmetic.ExactOps
import qualified Numeric.AERN.RealArithmetic.NumericOrderRounding as ArithUpDn

import Data.Word

--import Foreign.C.String(CString)
--import Foreign.C.Types(CULong, CLong, CInt, CUInt, CDouble, CChar)
import Foreign.Ptr(Ptr)
--import Foreign.Marshal(alloca)
import Foreign.Storable
import Foreign.Marshal.Alloc (malloc,finalizerFree)
import Foreign.StablePtr (StablePtr, newStablePtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr) --, mallocForeignPtrBytes)

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
peekSizes      :: ForeignPtr (Poly cf) -> IO (Var, Size)
peekSizes fp =
    withForeignPtr fp $ \ptr -> 
            do
            maxArityC <- #{peek Poly, maxArity} ptr
            maxSizeC <- #{peek Poly, maxSize} ptr
            return (fromCVar maxArityC, fromCSize maxSizeC)            

data Ops t =
    Ops
    {
        ops_zero :: {-# UNPACK #-} ! (StablePtr t),
        ops_one :: {-# UNPACK #-} ! (StablePtr t),
        ops_absUp :: {-# UNPACK #-} ! (StablePtr (UnaryOp t)),
        ops_absDn :: {-# UNPACK #-} ! (StablePtr (UnaryOp t)),
        ops_plusUp :: {-# UNPACK #-} ! (StablePtr (Op t)),
        ops_plusDn :: {-# UNPACK #-} ! (StablePtr (Op t)),
        ops_timesUp :: {-# UNPACK #-} ! (StablePtr (Op t)),
        ops_timesDn :: {-# UNPACK #-} ! (StablePtr (Op t))
    }

opsRealArithmeticBasis :: 
    (ArithUpDn.RoundedReal t) => 
    t ->
    ArithUpDn.RoundedRealEffortIndicator t -> 
    IO (Ops t)
opsRealArithmeticBasis sample effort =
    do
    zeroSP <- newStablePtr zero
    oneSP <- newStablePtr one
    let absEffort = ArithUpDn.rrEffortAbs sample effort
    absUpSP <- newStablePtr $ ArithUpDn.absUpEff absEffort 
    absDnSP <- newStablePtr $ ArithUpDn.absDnEff absEffort
    let fldEffort = ArithUpDn.rrEffortField sample effort
    let addEffort = ArithUpDn.fldEffortAdd sample fldEffort 
    addUpSP <- newStablePtr $ ArithUpDn.addUpEff addEffort  
    addDnSP <- newStablePtr $ ArithUpDn.addDnEff addEffort
    let multEffort = ArithUpDn.fldEffortMult sample fldEffort
    multUpSP <- newStablePtr $ ArithUpDn.multUpEff multEffort  
    multDnSP <- newStablePtr $ ArithUpDn.multDnEff multEffort
    return $
      Ops
        zeroSP oneSP 
        absUpSP absDnSP
        addUpSP addDnSP
        multUpSP multDnSP

instance (Storable cf) => Storable (Ops cf) where
    sizeOf _ = #size Ops
    alignment _ = #{alignment Ops}
    peek = error "Ops.peek: Not needed and not applicable"
    poke ptr (Ops zero one absUp absDn plusUp plusDn timesUp timesDn) = 
        do 
        #{poke Ops, zero} ptr zero
        #{poke Ops, one} ptr one
        #{poke Ops, absUp} ptr absUp
        #{poke Ops, absDn} ptr absDn
        #{poke Ops, plusUp} ptr plusUp
        #{poke Ops, plusDn} ptr plusDn
        #{poke Ops, timesUp} ptr timesUp
        #{poke Ops, timesDn} ptr timesDn

----------------------------------------------------------------

foreign import ccall unsafe "newConstPoly"
        poly_newConstPoly :: (StablePtr cf) -> CVar -> CSize -> IO (Ptr (Poly cf))  

        
newConstPoly :: cf -> Var -> Size -> IO (ForeignPtr (Poly cf))
newConstPoly c maxArity maxSize =
    do
    cSP <- newStablePtr c
    pP <- poly_newConstPoly cSP (toCVar maxArity) (toCSize maxSize)
    newForeignPtr finalizerFree pP

----------------------------------------------------------------

foreign import ccall unsafe "newProjectionPoly"
        poly_newProjectionPoly :: 
            (Ptr (Ops cf)) -> CVar -> CVar -> CSize -> IO (Ptr (Poly cf))  

        
newProjectionPoly :: 
    (Storable cf) => 
    (Ops cf) -> Var -> Var -> Size -> IO (ForeignPtr (Poly cf))
newProjectionPoly ops x maxArity maxSize =
    do
    opsP <- malloc
    poke opsP ops
    pP <- poly_newProjectionPoly opsP (toCVar x) (toCVar maxArity) (toCSize maxSize)
    newForeignPtr finalizerFree pP
    