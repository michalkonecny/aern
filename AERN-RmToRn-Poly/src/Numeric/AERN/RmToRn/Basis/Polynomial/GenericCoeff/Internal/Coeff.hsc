{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
#include <GenericCoeff/poly.h>
{-|
    Module      :  Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Coeff
    Description :  Haskell's coefficient ops for use within C
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Haskell interface to C polynomials with Haskell coefficients.
-}

module Numeric.AERN.RmToRn.Basis.Polynomial.GenericCoeff.Internal.Coeff
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

type OpsFP cf = ForeignPtr (DummyOps cf)
data DummyOps cf -- avoiding the s parameter of ST
type OpsPtr cf = Ptr (DummyOps cf) -- for foreign import declarations

data Ops s cf =
    Ops
    {
        ops_zero :: {-# UNPACK #-} ! (StablePtr cf),
        ops_one :: {-# UNPACK #-} ! (StablePtr cf),
        ops_new :: {-# UNPACK #-} ! (StablePtr (NewOpMutable s cf)),
        ops_clone :: {-# UNPACK #-} ! (StablePtr (CloneOpMutable s cf)),
        ops_assign :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s cf)),
        ops_assignFromPure :: {-# UNPACK #-} ! (StablePtr (UnaryFromPureOpMutable s cf)),
        ops_compare :: {-# UNPACK #-} ! (StablePtr (ComparisonOpMutable s cf)),
        ops_negMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s cf)),
        ops_absUpMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s cf)),
        ops_absDnMutable :: {-# UNPACK #-} ! (StablePtr (UnaryOpMutable s cf)),
        ops_plusUpMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_plusDnMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_minusUpMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_minusDnMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_timesUpMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_timesDnMutable :: {-# UNPACK #-} ! (StablePtr (BinaryOpMutable s cf)),
        ops_timesByIntUpMutable :: {-# UNPACK #-} ! (StablePtr (MixedIntOpMutable s cf)),
        ops_timesByIntDnMutable :: {-# UNPACK #-} ! (StablePtr (MixedIntOpMutable s cf)),
        ops_divByIntUpMutable :: {-# UNPACK #-} ! (StablePtr (MixedIntOpMutable s cf)),
        ops_divByIntDnMutable :: {-# UNPACK #-} ! (StablePtr (MixedIntOpMutable s cf))
    }

instance (Storable cf) => Storable (Ops s cf) where
    sizeOf _ = #size Ops
    alignment _ = #{alignment Ops}
    peek = error "Ops.peek: Not needed and not applicable"
    poke ptr 
            (Ops
              zero one new clone assign assignFromPure compare
              negMutable
              absUpMutable absDnMutable 
              plusUpMutable plusDnMutable 
              minusUpMutable minusDnMutable 
              timesUpMutable timesDnMutable
              timesByIntUpMutable timesByIntDnMutable
              divByIntUpMutable divByIntDnMutable
            ) = 
        do 
        #{poke Ops, zero} ptr zero
        #{poke Ops, one} ptr one
        #{poke Ops, new} ptr new
        #{poke Ops, clone} ptr clone
        #{poke Ops, assign} ptr assign
        #{poke Ops, assignFromPure} ptr assignFromPure
        #{poke Ops, compare} ptr compare
        #{poke Ops, negMutable} ptr negMutable
        #{poke Ops, absUpMutable} ptr absUpMutable
        #{poke Ops, absDnMutable} ptr absDnMutable
        #{poke Ops, plusUpMutable} ptr plusUpMutable
        #{poke Ops, plusDnMutable} ptr plusDnMutable
        #{poke Ops, minusUpMutable} ptr minusUpMutable
        #{poke Ops, minusDnMutable} ptr minusDnMutable
        #{poke Ops, timesUpMutable} ptr timesUpMutable
        #{poke Ops, timesDnMutable} ptr timesDnMutable
        #{poke Ops, timesByIntUpMutable} ptr timesByIntUpMutable
        #{poke Ops, timesByIntDnMutable} ptr timesByIntDnMutable
        #{poke Ops, divByIntUpMutable} ptr divByIntUpMutable
        #{poke Ops, divByIntDnMutable} ptr divByIntDnMutable

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
    cf ->
    (NewOpMutable s cf) ->
    (CloneOpMutable s cf) ->
    (UnaryOpMutable s cf) ->
    (UnaryFromPureOpMutable s cf) ->
    (ComparisonOpMutable s cf) ->
    (UnaryOpMutable s cf) ->
    (UnaryOpMutable s cf) ->
    (UnaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (BinaryOpMutable s cf) ->
    (MixedIntOpMutable s cf) ->
    (MixedIntOpMutable s cf) ->
    (MixedIntOpMutable s cf) ->
    (MixedIntOpMutable s cf) ->
    IO (Ops s cf)
mkOps zero one new clone assign assignFromPure compare 
        neg absUp absDn addUp addDn subtrUp subtrDn 
        multUp multDn 
        multByIntUp multByIntDn 
        divByIntUp divByIntDn 
    =
    do
    zeroSP <- newStablePtr zero
    oneSP <- newStablePtr one
    newSP <- newStablePtr new
    cloneSP <- newStablePtr clone  
    assignSP <- newStablePtr assign  
    assignFromPureSP <- newStablePtr assignFromPure  
    compareSP <- newStablePtr compare  
    negSP <- newStablePtr neg  
    absUpSP <- newStablePtr absUp  
    absDnSP <- newStablePtr absDn
    addUpSP <- newStablePtr addUp   
    subtrDnSP <- newStablePtr subtrDn
    subtrUpSP <- newStablePtr subtrUp   
    addDnSP <- newStablePtr addDn
    multUpSP <- newStablePtr multUp   
    multDnSP <- newStablePtr multDn
    multByIntUpSP <- newStablePtr multByIntUp   
    multByIntDnSP <- newStablePtr multByIntDn
    divByIntUpSP <- newStablePtr divByIntUp   
    divByIntDnSP <- newStablePtr divByIntDn
    return $
      Ops
        zeroSP oneSP 
        newSP cloneSP assignSP assignFromPureSP
        compareSP
        negSP
        absUpSP absDnSP
        addUpSP addDnSP
        subtrUpSP subtrDnSP
        multUpSP multDnSP
        multByIntUpSP multByIntDnSP
        divByIntUpSP divByIntDnSP

mkOpsArithUpDn ::
    (ArithUpDn.RoundedRealInPlace cf, 
     CanBeMutable cf) 
    => 
    cf ->
    ArithUpDn.RoundedRealEffortIndicator cf -> 
    IO (Ops s cf)
mkOpsArithUpDn sample effort =
    let absEffort = ArithUpDn.rrEffortAbs sample effort in
    let fldEffort = ArithUpDn.rrEffortField sample effort in
    let fldByIntEffort = ArithUpDn.rrEffortIntMixedField sample effort in
    let addEffort = ArithUpDn.fldEffortAdd sample fldEffort in
    let multEffort = ArithUpDn.fldEffortMult sample fldEffort in
    let multByIntEffort = ArithUpDn.mxfldEffortMult sample (0::Int) fldByIntEffort in
    let divByIntEffort = ArithUpDn.mxfldEffortDiv sample (0::Int) fldByIntEffort in
    mkOps
        z o
        (makeMutable)
        (cloneMutable)
        (assignMutable)
        (writeMutable)
        (compareMutable)
        (negInPlace)
        (ArithUpDn.absUpInPlaceEff absEffort)
        (ArithUpDn.absDnInPlaceEff absEffort)
        (ArithUpDn.addUpInPlaceEff addEffort)
        (ArithUpDn.addDnInPlaceEff addEffort)
        (ArithUpDn.subtrUpInPlaceEff addEffort)
        (ArithUpDn.subtrDnInPlaceEff addEffort)
        (ArithUpDn.multUpInPlaceEff multEffort)
        (ArithUpDn.multDnInPlaceEff multEffort)
        (ArithUpDn.mixedMultUpInPlaceEff multByIntEffort)
        (ArithUpDn.mixedMultDnInPlaceEff multByIntEffort)
        (ArithUpDn.mixedDivUpInPlaceEff divByIntEffort)
        (ArithUpDn.mixedDivDnInPlaceEff divByIntEffort)
    where
    z = zero
    o = one
    _ = [z,sample]
    compareMutable v1M v2M =
        unsafePerformIO $ unsafeSTToIO $
        do
        v1 <- unsafeReadMutable v1M
        v2 <- unsafeReadMutable v2M
        let _ = [v1,v2,sample] 
        return $ NumOrd.pCompareEff (NumOrd.pCompareDefaultEffort sample) v1 v2

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

