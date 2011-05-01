{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.FieldOps
    Description :  rounded basic arithmetic operations  
    Copyright   :  (c) Michal Konecny, Jan Duracz
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable
    
    In-place versions of rounded basic arithmetic operations.
    
    Each operations takes mutable parameters instead of pure parameters
    and has one extra mutable parameter before the other parameters, 
    in which it stores the result.
    The mutable parameters can alias arbitrarily, making it possible
    to eg add to a number overwriting the original number.
    
    The operations have as their first paramter a non-mutable sample value
    to aid type-checking, ie to help work out which type the mutable parameters
    contain.
    
    This module is hidden and reexported via its parent RefinementOrderRounding.InPlace. 
-}
module Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.FieldOps 
where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps

import Numeric.AERN.RealArithmetic.Auxiliary
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion

import Numeric.AERN.Basics.Effort
import Numeric.AERN.Basics.Exception (HasLegalValues)
import Numeric.AERN.Basics.Mutable
import Numeric.AERN.RealArithmetic.Laws
import Numeric.AERN.RealArithmetic.Measures
import qualified Numeric.AERN.Basics.NumericOrder as NumOrd
import qualified Numeric.AERN.Basics.RefinementOrder as RefOrd
import Numeric.AERN.Basics.RefinementOrder.OpsImplicitEffort

import Test.QuickCheck
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Monad.ST
import Data.Maybe

class (RoundedAddEffort t, CanBeMutable t) => RoundedAddInPlace t where
    addInInPlaceEff :: OpMutable2Eff (AddEffortIndicator t) t s
    addOutInPlaceEff :: OpMutable2Eff (AddEffortIndicator t) t s

addInInPlaceEffFromPure,
 addOutInPlaceEffFromPure ::
    (CanBeMutable t, RoundedAdd t) =>
    OpMutable2Eff (AddEffortIndicator t) t s
addInInPlaceEffFromPure = pureToMutable2Eff addInEff 
addOutInPlaceEffFromPure = pureToMutable2Eff addOutEff 

addInInPlaceEffFromInPlace,
 addOutInPlaceEffFromInPlace :: 
    (RoundedAddInPlace t) =>
    (AddEffortIndicator t) -> t -> t -> t
addInInPlaceEffFromInPlace = mutable2EffToPure addInInPlaceEff 
addOutInPlaceEffFromInPlace = mutable2EffToPure addOutInPlaceEff 

propInOutAddInPlace ::
    (RefOrd.PartialComparison t, 
     RoundedAddInPlace t, 
     RoundedAdd t, 
     Neg t,
     Show t, HasLegalValues t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedPair t) -> Bool
propInOutAddInPlace sample initEffort (RefOrd.UniformlyOrderedPair (e1, e2)) =
    roundedInPlace2ConsistentWithPure "addition"
        addInInPlaceEff addOutInPlaceEff addInEff addOutEff
        RefOrd.pLeqEff initEffort
        e1 e2

class (RoundedAddInPlace t, NegInPlace t) => RoundedSubtrInPlace t where
    subtrInInPlaceEff :: OpMutable2Eff (AddEffortIndicator t) t s
    subtrOutInPlaceEff :: OpMutable2Eff (AddEffortIndicator t) t s
    subtrInInPlaceEff effort rM aM bM =
        do
        bbM <- cloneMutable bM
        negInPlace bbM bM
        addInInPlaceEff effort rM aM bbM
    subtrOutInPlaceEff effort rM aM bM = 
        do
        bbM <- cloneMutable bM
        negInPlace bbM bM
        addOutInPlaceEff effort rM aM bbM

propInOutSubtrInPlace ::
    (RefOrd.PartialComparison t, 
     RoundedSubtrInPlace t, 
     RoundedSubtr t, 
     Neg t,
     Show t, HasLegalValues t,
     Show (AddEffortIndicator t),
     EffortIndicator (AddEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AddEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedPair t) -> Bool
propInOutSubtrInPlace sample initEffort (RefOrd.UniformlyOrderedPair (e1, e2)) =
    roundedInPlace2ConsistentWithPure "subtraction"
        subtrInInPlaceEff subtrOutInPlaceEff subtrInEff subtrOutEff
        RefOrd.pLeqEff initEffort
        e1 e2

class (RoundedAbs t, CanBeMutable t) => RoundedAbsInPlace t where
    absInInPlaceEff :: OpMutable1Eff (AbsEffortIndicator t) t s
    absOutInPlaceEff :: OpMutable1Eff (AbsEffortIndicator t) t s
    absInInPlaceEff = pureToMutable1Eff absInEff 
    absOutInPlaceEff = pureToMutable1Eff absOutEff 

propInOutAbsInPlace ::
    (RefOrd.PartialComparison t, RoundedAbsInPlace t, Neg t,
     Show t, HasLegalValues t,
     Show (AbsEffortIndicator t),
     EffortIndicator (AbsEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     AbsEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> Bool
propInOutAbsInPlace sample initEffort (RefOrd.UniformlyOrderedSingleton e) =
    roundedInPlace1ConsistentWithPure "abs"
        absInInPlaceEff absOutInPlaceEff absInEff absOutEff
        RefOrd.pLeqEff initEffort
        e


class (RoundedMultiplyEffort t, CanBeMutable t) => RoundedMultiplyInPlace t where
    multInInPlaceEff :: OpMutable2Eff (MultEffortIndicator t) t s
    multOutInPlaceEff :: OpMutable2Eff (MultEffortIndicator t) t s

multInInPlaceEffFromPure,
 multOutInPlaceEffFromPure ::
    (CanBeMutable t, RoundedMultiply t) =>
    OpMutable2Eff (MultEffortIndicator t) t s
multInInPlaceEffFromPure = pureToMutable2Eff multInEff 
multOutInPlaceEffFromPure = pureToMutable2Eff multOutEff 

multInInPlaceEffFromInPlace,
 multOutInPlaceEffFromInPlace ::
    (RoundedMultiplyInPlace t) =>
    (MultEffortIndicator t) -> t -> t -> t
multInInPlaceEffFromInPlace = mutable2EffToPure multInInPlaceEff 
multOutInPlaceEffFromInPlace = mutable2EffToPure multOutInPlaceEff 

propInOutMultInPlace ::
    (RefOrd.PartialComparison t, 
     RoundedMultiplyInPlace t, 
     RoundedMultiply t, 
     Neg t,
     Show t, HasLegalValues t,
     Show (MultEffortIndicator t),
     EffortIndicator (MultEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     MultEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedPair t) -> Bool
propInOutMultInPlace sample initEffort (RefOrd.UniformlyOrderedPair (e1, e2)) =
    roundedInPlace2ConsistentWithPure "multiplication"
        multInInPlaceEff multOutInPlaceEff multInEff multOutEff
        RefOrd.pLeqEff initEffort
        e1 e2

powerToNonnegIntInInPlaceEffFromMult ::
    (RoundedMultiplyInPlace t, HasOne t) =>
    OpMutableNonmutEff (PowerToNonnegIntEffortIndicatorFromMult t) t Int s 
powerToNonnegIntInInPlaceEffFromMult effMult rM eM n =
    powerFromMultInPlace (multInInPlaceEff effMult) rM eM n

powerToNonnegIntOutInPlaceEffFromMult ::
    (RoundedMultiplyInPlace t, HasOne t) =>
    OpMutableNonmutEff (PowerToNonnegIntEffortIndicatorFromMult t) t Int s 
powerToNonnegIntOutInPlaceEffFromMult effMult rM eM n =
    powerFromMultInPlace (multOutInPlaceEff effMult) rM eM n


class (RoundedPowerToNonnegIntEffort t, CanBeMutable t) => 
    RoundedPowerToNonnegIntInPlace t 
    where
    powerToNonnegIntInInPlaceEff ::
        OpMutableNonmutEff (PowerToNonnegIntEffortIndicator t) t Int s
    powerToNonnegIntOutInPlaceEff ::
        OpMutableNonmutEff (PowerToNonnegIntEffortIndicator t) t Int s

powerToNonnegIntInInPlaceEffFromPure,
 powerToNonnegIntOutInPlaceEffFromPure ::
    (CanBeMutable t, RoundedPowerToNonnegInt t) =>
    OpMutableNonmutEff (PowerToNonnegIntEffortIndicator t) t Int s
powerToNonnegIntInInPlaceEffFromPure =
    pureToMutableNonmutEff powerToNonnegIntInEff 
powerToNonnegIntOutInPlaceEffFromPure =
    pureToMutableNonmutEff powerToNonnegIntOutEff 

powerToNonnegIntInInPlaceEffFromInPlace,
 powerToNonnegIntOutInPlaceEffFromInPlace ::
    (RoundedPowerToNonnegIntInPlace t) =>
    (PowerToNonnegIntEffortIndicator t) -> t -> Int -> t
powerToNonnegIntInInPlaceEffFromInPlace = 
    mutableNonmutEffToPure powerToNonnegIntInInPlaceEff 
powerToNonnegIntOutInPlaceEffFromInPlace = 
    mutableNonmutEffToPure powerToNonnegIntOutInPlaceEff

propInOutPowerToNonnegInPlace ::
    (RefOrd.PartialComparison t, 
     RoundedPowerToNonnegIntInPlace t, 
     RoundedPowerToNonnegInt t, 
     Neg t,
     Show t, HasLegalValues t,
     Show (PowerToNonnegIntEffortIndicator t),
     EffortIndicator (PowerToNonnegIntEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     PowerToNonnegIntEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedSingleton t) -> Int -> Bool
propInOutPowerToNonnegInPlace sample initEffort (RefOrd.UniformlyOrderedSingleton e) n =
    roundedInPlace1ConsistentWithPure "non-neg integer power"
        (\eff r e -> powerToNonnegIntInInPlaceEff eff r e n) 
        (\eff r e -> powerToNonnegIntOutInPlaceEff eff r e n) 
        (\eff e -> powerToNonnegIntInEff eff e n) 
        (\eff e -> powerToNonnegIntOutEff eff e n)
        RefOrd.pLeqEff initEffort
        e

class (HasOne t, RoundedDivideEffort t, CanBeMutable t) => RoundedDivideInPlace t where
    divInInPlaceEff :: OpMutable2Eff (DivEffortIndicator t) t s
    divOutInPlaceEff :: OpMutable2Eff (DivEffortIndicator t) t s
    recipInInPlaceEff :: OpMutable1Eff (DivEffortIndicator t) t s
    recipOutInPlaceEff :: OpMutable1Eff (DivEffortIndicator t) t s
    
    recipInInPlaceEff effort resM aM =
        do
        oneM <- unsafeMakeMutable one
        divInInPlaceEff effort resM oneM aM
    recipOutInPlaceEff effort resM aM =
        do
        oneM <- unsafeMakeMutable one
        divOutInPlaceEff effort resM oneM aM

divInInPlaceEffFromPure,
 divOutInPlaceEffFromPure ::
    (CanBeMutable t, RoundedDivide t) =>
    OpMutable2Eff (DivEffortIndicator t) t s
divInInPlaceEffFromPure = pureToMutable2Eff divInEff 
divOutInPlaceEffFromPure = pureToMutable2Eff divOutEff 

divInInPlaceEffFromInPlace,
 divOutInPlaceEffFromInPlace :: 
    (RoundedDivideInPlace t) =>
    (DivEffortIndicator t) -> t -> t -> t 
divInInPlaceEffFromInPlace = mutable2EffToPure divInInPlaceEff 
divOutInPlaceEffFromInPlace = mutable2EffToPure divOutInPlaceEff 

recipInInPlaceEffFromPure,
 recipOutInPlaceEffFromPure ::
    (CanBeMutable t, RoundedDivide t) =>
    OpMutable1Eff (DivEffortIndicator t) t s
recipInInPlaceEffFromPure = pureToMutable1Eff recipInEff 
recipOutInPlaceEffFromPure = pureToMutable1Eff recipOutEff 

recipInInPlaceEffFromInPlace,
 recipOutInPlaceEffFromInPlace ::
    (RoundedDivideInPlace t) =>
    (DivEffortIndicator t) -> t -> t
recipInInPlaceEffFromInPlace = mutable1EffToPure recipInInPlaceEff 
recipOutInPlaceEffFromInPlace = mutable1EffToPure recipOutInPlaceEff 

propInOutDivInPlace ::
    (RefOrd.PartialComparison t, 
     RoundedDivideInPlace t, 
     RoundedDivide t, 
     Neg t,
     Show t, HasZero t, HasLegalValues t,
     Show (DivEffortIndicator t),
     EffortIndicator (DivEffortIndicator t),
     Show (RefOrd.PartialCompareEffortIndicator t),
     EffortIndicator (RefOrd.PartialCompareEffortIndicator t)
     ) =>
    t ->
    (RefOrd.PartialCompareEffortIndicator t, 
     DivEffortIndicator t) -> 
    (RefOrd.UniformlyOrderedPair t) -> Bool
propInOutDivInPlace sample initEffort@(effComp, _) (RefOrd.UniformlyOrderedPair (e1, e2)) =
    let ?pCompareEffort = effComp in
    case (e2 ⊑? zero, zero ⊑? e2) of
        (Just False, Just False) ->
            roundedInPlace2ConsistentWithPure "division"
                divInInPlaceEff divOutInPlaceEff divInEff divOutEff
                RefOrd.pLeqEff initEffort
                e1 e2
        _ -> True 

testsInOutFieldOpsInPlace (name, sample) =
    testGroup (name ++ " in-place up/down rounded ops match pure ops") $
        [
            testProperty "addition" (propInOutAddInPlace sample)
        ,
            testProperty "subtraction" (propInOutSubtrInPlace sample)
        ,
            testProperty "absolute value" (propInOutAbsInPlace sample)
        ,
            testProperty "multiplication" (propInOutMultInPlace sample)
        ,
            testProperty "integer power" (propInOutMultInPlace sample)
        ,
            testProperty "division" (propInOutDivInPlace sample)
        ]


class 
        (RoundedSubtrInPlace t, 
         RoundedMultiplyInPlace t, 
         RoundedRingEffort t) => 
    RoundedRingInPlace t

class
        (RoundedRingInPlace t,
         RoundedDivideInPlace t,
         RoundedFieldEffort t) => 
    RoundedFieldInPlace t

    