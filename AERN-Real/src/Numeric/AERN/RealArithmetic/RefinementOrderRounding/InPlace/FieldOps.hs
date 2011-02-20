{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-|
    Module      :  Numeric.AERN.RealArithmetic.RefinementOrderRounding.InPlace.FieldOps
    Description :  rounded basic arithmetic operations  
    Copyright   :  (c) Michal Konecny
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
(
    RoundedAddInPlace(..), 
    RoundedSubtrInPlace(..),
    RoundedAbsInPlace(..),
    RoundedMultiplyInPlace(..),
    RoundedPowerToNonnegIntInPlace(..),
    powerToNonnegIntInInPlaceEffFromMult,
    powerToNonnegIntOutInPlaceEffFromMult,
    RoundedDivideInPlace(..),
    testsInOutFieldOpsInPlace,
    RoundedRingInPlace(..), RoundedFieldInPlace(..)
)
where

import Prelude hiding (EQ, LT, GT)
import Numeric.AERN.Basics.PartialOrdering

import Numeric.AERN.RealArithmetic.RefinementOrderRounding.FieldOps

import Numeric.AERN.RealArithmetic.Auxiliary
import Numeric.AERN.RealArithmetic.ExactOps
import Numeric.AERN.RealArithmetic.RefinementOrderRounding.Conversion

import Numeric.AERN.Basics.Effort
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

class (RoundedAdd t, CanBeMutable t) => RoundedAddInPlace t where
    addInInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    addOutInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    addInInPlaceEff sample = pureToMutable2Eff sample addInEff 
    addOutInPlaceEff sample = pureToMutable2Eff sample addOutEff 

propInOutAddInPlace ::
    (RefOrd.PartialComparison t, RoundedAddInPlace t, Neg t,
     Show t,
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
    roundedInPlace2ConsistentWithPure
        (addInInPlaceEff sample) (addOutInPlaceEff sample) addInEff addOutEff
        RefOrd.pLeqEff initEffort
        e1 e2

class (RoundedAddInPlace t,  RoundedSubtr t, NegInPlace t) => RoundedSubtrInPlace t where
    subtrInInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    subtrOutInPlaceEff :: t -> OpMutable2Eff (AddEffortIndicator t) t s
    subtrInInPlaceEff sample effort rM aM bM =
        do
        bbM <- cloneMutable sample bM
        negInPlace sample bbM bM
        addInInPlaceEff sample effort rM aM bbM
    subtrOutInPlaceEff sample effort rM aM bM = 
        do
        bbM <- cloneMutable sample bM
        negInPlace sample bbM bM
        addOutInPlaceEff sample effort rM aM bbM

propInOutSubtrInPlace ::
    (RefOrd.PartialComparison t, RoundedSubtrInPlace t, Neg t,
     Show t,
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
    roundedInPlace2ConsistentWithPure
        (subtrInInPlaceEff sample) (subtrOutInPlaceEff sample) subtrInEff subtrOutEff
        RefOrd.pLeqEff initEffort
        e1 e2

class (RoundedAbs t, CanBeMutable t) => RoundedAbsInPlace t where
    absInInPlaceEff :: t -> OpMutable1Eff (AbsEffortIndicator t) t s
    absOutInPlaceEff :: t -> OpMutable1Eff (AbsEffortIndicator t) t s
    absInInPlaceEff sample = pureToMutable1Eff sample absInEff 
    absOutInPlaceEff sample = pureToMutable1Eff sample absOutEff 

propInOutAbsInPlace ::
    (RefOrd.PartialComparison t, RoundedAbsInPlace t, Neg t,
     Show t,
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
    roundedInPlace1ConsistentWithPure
        (absInInPlaceEff sample) (absOutInPlaceEff sample) absInEff absOutEff
        RefOrd.pLeqEff initEffort
        e


class (RoundedMultiply t, CanBeMutable t) => RoundedMultiplyInPlace t where
    multInInPlaceEff :: t -> OpMutable2Eff (MultEffortIndicator t) t s
    multOutInPlaceEff :: t -> OpMutable2Eff (MultEffortIndicator t) t s
    multInInPlaceEff sample = pureToMutable2Eff sample multInEff 
    multOutInPlaceEff sample = pureToMutable2Eff sample multOutEff 

propInOutMultInPlace ::
    (RefOrd.PartialComparison t, RoundedMultiplyInPlace t, Neg t,
     Show t,
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
    roundedInPlace2ConsistentWithPure
        (multInInPlaceEff sample) (multOutInPlaceEff sample) multInEff multOutEff
        RefOrd.pLeqEff initEffort
        e1 e2

powerToNonnegIntInInPlaceEffFromMult ::
    (RoundedMultiplyInPlace t, HasOne t) =>
    t ->
    OpMutableNonmutEff (PowerToNonnegIntEffortIndicatorFromMult t) t Int s 
powerToNonnegIntInInPlaceEffFromMult sample effMult rM eM n =
    powerFromMultInPlace sample (multInInPlaceEff sample effMult) rM eM n

powerToNonnegIntOutInPlaceEffFromMult ::
    (RoundedMultiplyInPlace t, HasOne t) =>
    t ->
    OpMutableNonmutEff (PowerToNonnegIntEffortIndicatorFromMult t) t Int s 
powerToNonnegIntOutInPlaceEffFromMult sample effMult rM eM n =
    powerFromMultInPlace sample (multOutInPlaceEff sample effMult) rM eM n


class (RoundedPowerToNonnegInt t, CanBeMutable t) => RoundedPowerToNonnegIntInPlace t where
    powerToNonnegIntInInPlaceEff ::
        t -> OpMutableNonmutEff (PowerToNonnegIntEffortIndicator t) t Int s
    powerToNonnegIntOutInPlaceEff ::
        t -> OpMutableNonmutEff (PowerToNonnegIntEffortIndicator t) t Int s
    powerToNonnegIntInInPlaceEff sample =
        pureToMutableNonmutEff sample powerToNonnegIntInEff 
    powerToNonnegIntOutInPlaceEff sample =
        pureToMutableNonmutEff sample powerToNonnegIntOutEff 

propInOutPowerToNonnegInPlace ::
    (RefOrd.PartialComparison t, RoundedPowerToNonnegIntInPlace t, Neg t,
     Show t,
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
    roundedInPlace1ConsistentWithPure
        (\eff r e -> powerToNonnegIntInInPlaceEff sample eff r e n) 
        (\eff r e -> powerToNonnegIntOutInPlaceEff sample eff r e n) 
        (\eff e -> powerToNonnegIntInEff eff e n) 
        (\eff e -> powerToNonnegIntOutEff eff e n)
        RefOrd.pLeqEff initEffort
        e

class (RoundedDivide t, CanBeMutable t) => RoundedDivideInPlace t where
    divInInPlaceEff :: t -> OpMutable2Eff (DivEffortIndicator t) t s
    divOutInPlaceEff :: t -> OpMutable2Eff (DivEffortIndicator t) t s
    divInInPlaceEff sample = pureToMutable2Eff sample divInEff 
    divOutInPlaceEff sample = pureToMutable2Eff sample divOutEff 

propInOutDivInPlace ::
    (RefOrd.PartialComparison t, RoundedDivideInPlace t, Neg t,
     Show t, HasZero t,
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
            roundedInPlace2ConsistentWithPure
                (divInInPlaceEff sample) (divOutInPlaceEff sample) divInEff divOutEff
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


class (RoundedSubtrInPlace t, RoundedMultiplyInPlace t) => RoundedRingInPlace t
class (RoundedRingInPlace t, RoundedDivideInPlace t) => RoundedFieldInPlace t

    