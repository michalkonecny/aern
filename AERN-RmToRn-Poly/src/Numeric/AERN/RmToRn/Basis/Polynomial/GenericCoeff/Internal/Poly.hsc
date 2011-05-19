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
import Control.Monad.ST (ST, runST, unsafeIOToST, unsafeSTToIO)

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
data Poly cf = 
    Poly 
        {
            poly_ops :: OpsFP cf, -- coefficient operations to pass to some C functions 
            poly_poly :: PolyFP cf -- a polynomial structure
        } 
        
type OpsFP cf = ForeignPtr (DummyOps cf)
data DummyOps cf -- avoiding the s parameter of ST

type OpsPtr cf = Ptr (DummyOps cf) -- for foreign import declarations

type PolyFP cf = ForeignPtr (CPoly cf)
data CPoly cf -- transparent to Haskell - details available in C

instance 
    (CanBeMutable cf, ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) => 
    CanBeMutable (Poly cf) 
    where
    newtype Mutable (Poly cf) s = PolyM (Poly cf) -- (Mutable cf s))
    unsafeMakeMutable (Poly c p) = return $ PolyM (Poly c p)
    unsafeReadMutable (PolyM (Poly c p)) = return (Poly c p)
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
        (arity, size, deg) <- peekSizesM pM
        resM <- constPolyM opsFP arity size deg zero zero
        assignMutable resM pM
        return resM
    
type PolyM cf s = Mutable (Poly cf) s

{-# INLINE peekSizes #-}
peekSizes :: (Poly cf) -> (Var, Size, Power)
peekSizes p =
    unsafePerformIO $ peekSizesIO p

{-# INLINE peekSizesM #-}
peekSizesM :: (PolyM cf s) -> ST s (Var, Size, Power)
peekSizesM (PolyM p) =
    unsafeIOToST $ peekSizesIO p

{-# INLINE peekSizesIO #-}
peekSizesIO :: (Poly cf) -> IO (Var, Size, Power)
peekSizesIO (Poly opsFP polyFP) =
    withForeignPtr polyFP $ \ptr -> 
        do
        maxArityC <- #{peek Poly, maxArity} ptr
        maxSizeC <- #{peek Poly, maxSize} ptr
        maxDegreeC <- #{peek Poly, maxDeg} ptr
        return (fromCVar maxArityC, fromCSize maxSizeC, fromCPower maxDegreeC)            

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

newOpsFP ::
    (Storable cf) =>
    (Ops s cf) ->
    IO (OpsFP cf)
newOpsFP ops =
    do
    ptr <- malloc
    poke ptr ops
    fp <- Conc.newForeignPtr ptr (free ptr) 
    return $ castForeignPtr fp
    
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

opsFPArithUpDn ::
    (ArithUpDn.RoundedRealInPlace cf, CanBeMutable cf, Storable cf) => 
    cf ->
    ArithUpDn.RoundedRealEffortIndicator cf -> 
    OpsFP cf
opsFPArithUpDn sample effort =
    unsafePerformIO $
        do
        ops <- mkOpsArithUpDn sample effort
        newOpsFP ops

opsFPArithUpDnDefaultEffort ::
    (ArithUpDn.RoundedRealInPlace cf, CanBeMutable cf, Storable cf) => 
    cf ->
    OpsFP cf
opsFPArithUpDnDefaultEffort sample =
    opsFPArithUpDn sample (ArithUpDn.roundedRealDefaultEffort sample)

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
            CVar -> CSize -> CPower -> 
            IO (Ptr (CPoly cf))  

constPoly :: 
    (Show cf, CanBeMutable cf) => 
    OpsFP cf -> 
    Var -> Size -> Power ->
    cf -> cf -> 
    Poly cf
constPoly opsFP maxArity maxSize maxDeg c radius =
    unsafePerformIO $ constPolyIO opsFP maxArity maxSize maxDeg c radius

constPolyM :: 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    =>
    OpsFP cf -> 
    Var -> Size -> Power ->
    cf -> cf -> 
    ST s (PolyM cf s)
constPolyM opsFP maxArity maxSize maxDeg c radius =
    do
    p <- unsafeIOToST $ constPolyIO opsFP maxArity maxSize maxDeg c radius
    unsafeMakeMutable p

constPolyIO :: 
    (Show cf, CanBeMutable cf) => 
    OpsFP cf -> 
    Var -> Size -> Power -> 
    cf -> cf -> 
    IO (Poly cf)
constPolyIO opsFP maxArity maxSize maxDeg c radius =
    do
    cM <- unsafeSTToIO $ makeMutable c
    cSP <- newStablePtr cM
    radiusM <- unsafeSTToIO $ makeMutable radius
    radiusSP <- newStablePtr radiusM
--    putStrLn $ "calling newConstPoly for " ++ show c
    pP <- poly_newConstPoly cSP radiusSP (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
--    putStrLn $ "newConstPoly for " ++ show c ++ " returned"
    fp <- Conc.newForeignPtr pP (concFinalizerFreePoly pP)
    return $ Poly opsFP fp 

----------------------------------------------------------------

foreign import ccall unsafe "newProjectionPolyGenCf"
        poly_newProjectionPoly :: 
            (StablePtr (Mutable cf s)) -> 
            (StablePtr (Mutable cf s)) -> 
            (StablePtr (Mutable cf s)) -> 
            CVar -> CVar -> CSize -> CPower -> 
            IO (Ptr (CPoly cf))

projectionPoly :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) 
    => 
    OpsFP cf -> 
    Var -> Size -> Power -> 
    Var -> 
    Poly cf
projectionPoly opsFP maxArity maxSize maxDeg x =
    unsafePerformIO $ projectionPolyIO opsFP maxArity maxSize maxDeg x

projectionPolyM :: 
    (ArithUpDn.RoundedRealInPlace cf, Storable cf, Show cf) 
    =>
    OpsFP cf -> 
    Var -> Size -> Power -> 
    Var -> 
    ST s (PolyM cf s)
projectionPolyM opsFP maxArity maxSize maxDeg x =
    do
    p <- unsafeIOToST $ projectionPolyIO opsFP maxArity maxSize maxDeg x
    unsafeMakeMutable p

projectionPolyIO :: 
    (CanBeMutable cf, HasOne cf, HasZero cf) => 
    OpsFP cf -> 
    Var -> Size -> Power -> 
    Var -> 
    IO (Poly cf)
projectionPolyIO opsFP maxArity maxSize maxDeg x =
    do
    zeroM <- unsafeSTToIO $ makeMutable zero
    zeroSP <- newStablePtr zeroM
    oneM <- unsafeSTToIO $ makeMutable one
    oneSP <- newStablePtr oneM
    radM <- unsafeSTToIO $ makeMutable zero
    radSP <- newStablePtr radM
    pP <- poly_newProjectionPoly zeroSP oneSP radSP (toCVar x) (toCVar maxArity) (toCSize maxSize) (toCPower maxDeg)
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
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            OpsPtr cf ->
            (Ptr (CPoly cfm)) -> 
            (Ptr (CPoly cfm)) -> 
            IO ()

foreign import ccall safe "copyUpThinGenCf"
        poly_copyUpThin :: 
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (StablePtr cf) ->
            OpsPtr cf ->
            (Ptr (CPoly cfm)) -> 
            (Ptr (CPoly cfm)) -> 
            IO ()

foreign import ccall safe "copyDnThinGenCf"
        poly_copyDnThin :: 
            (StablePtr (ComparisonOp (Mutable cf s))) ->
            (StablePtr cf) ->
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
polyCopySameSizes (PolyM (Poly opsFP resFP)) (PolyM (Poly _ srcFP)) =
    do
    unsafeIOToST $
        do
        withForeignPtr resFP $ \resP ->
          withForeignPtr srcFP $ \srcP ->
             withForeignPtr opsFP $ \opsP ->
               poly_copySameSizes opsP resP srcP

------------------------------------------------------------------

polyCopyEncl ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyCopyEncl = 
    polyCopyOpMutable poly_copyEncl zero

polyCopyOpMutable copyOp sample (PolyM (Poly opsFP resFP)) (PolyM (Poly _ srcFP)) =
    do
    unsafeIOToST $
      do
      compareSP <- newStablePtr compareMutable
      _ <- withForeignPtr resFP $ \resP ->
             withForeignPtr srcFP $ \srcP ->
               withForeignPtr opsFP $ \opsP ->
                 copyOp compareSP opsP resP srcP
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
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyCopyUpThin sample = 
    polyCopyThinOpMutable poly_copyUpThin zero

polyCopyDnThin ::
    (Storable cf, CanBeMutable cf, HasZero cf, NumOrd.PartialComparison cf) =>
    cf ->
    PolyM cf s ->
    PolyM cf s ->
    ST s ()
polyCopyDnThin sample = 
    polyCopyThinOpMutable poly_copyDnThin zero

polyCopyThinOpMutable copyOp sample (PolyM (Poly opsFP resFP)) (PolyM (Poly _ srcFP)) =
    do
    unsafeIOToST $
      do
      zeroSP <- newStablePtr zero
      compareSP <- newStablePtr compareMutable
      _ <- withForeignPtr resFP $ \resP ->
             withForeignPtr srcFP $ \srcP ->
               withForeignPtr opsFP $ \opsP ->
                 copyOp compareSP zeroSP opsP resP srcP
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
       